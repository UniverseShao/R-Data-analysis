# 概览

- 目标：解释两类P值（“P for non-linearity”和“P for overall effect”）的含义、计算原理与解读；并说明“按加权百分位数设置RCS节点（knots）”的意义。
- 适用场景：复杂抽样设计（如NHANES）下的限制性立方样条（Restricted Cubic Spline, RCS）逻辑回归，使用 `survey::svyglm` 与 `anova.svyglm`。

术语与模型设定

- `svyglm`：在复杂抽样设计（权重、分层、整群）下拟合GLM，使用设计型（robust）方差估计。
- RCS（限制性立方样条）：在连续自变量上以若干“节点（knots）”拼接立方多项式，并在边界处添加线性约束，使曲线在两端线性、内部灵活。
- 嵌套模型比较：将“更复杂模型”与“简化模型”进行比较（Wald检验），得到近似F统计量与p值。

# 两类P值的定义与原理

1) P for non-linearity（非线性检验）

- 问题：自变量 `TyG_WWI` 与结局 `HF` 的关系是否需要非线性（样条项）？
- 原假设 H0：样条的非线性部分系数全为 0（模型退化为仅含“线性项”）。
- 模型比较：
  - 完整RCS模型：`HF ~ TyG_WWI_rcs + 协变量...`
  - 线性模型：`HF ~ TyG_WWI + 协变量...`
- 统计量：`anova(fit_rcs_model, fit_linear_model, method = "Wald")` 产出的设计型Wald统计量，近似服从F分布。p值由 `pf(F, df1, df2, lower.tail = FALSE)` 计算。
- 解读：p小（如 <0.05），说明“非线性部分”显著；p大则说明线性足够，RCS的灵活性未带来显著改进。

2) P for overall effect（整体效应检验）

- 问题：`TyG_WWI`（整体，包括线性项和样条项）是否与结局 `HF` 有关联？
- 原假设 H0：与 `TyG_WWI` 相关的所有系数（线性与样条基函数）全为 0。
- 模型比较：
  - 完整RCS模型：`HF ~ TyG_WWI_rcs + 协变量...`
  - 空模型（不含 `TyG_WWI`）：`HF ~ 协变量...`
- 统计量：`anova(fit_rcs_model, fit_null_model, method = "Wald")` 给出F与p值。
- 解读：p小表示总体上存在关联，即使“非线性”可能并不显著（可能是显著的线性效应）。

# 为何是Wald/F检验？

- 在复杂抽样下，标准误来自设计型估计（考虑权重、分层、整群）。此时 `anova.svyglm` 默认使用Wald检验并给出近似F统计量（带自由度 `df` 与设计自由度 `ddf`），p值通过F分布计算。
- 你的脚本中（见 `剂量分析(RSC)(去除极端值和不去除).R` 第100–140行）如 `anova` 返回的 `p` 为 `NULL`，就用 `pf(F, df, ddf, FALSE)` 计算：

```r
nonlinearity_test <- anova(fit_rcs_model, fit_linear_model, method = "Wald")
p_nonlinearity <- if (is.null(nonlinearity_test$p)) {
  pf(nonlinearity_test$F[2], nonlinearity_test$df[2], nonlinearity_test$ddf[2], lower.tail = FALSE)
} else nonlinearity_test$p[2]

overall_test <- anova(fit_rcs_model, fit_null_model, method = "Wald")
p_overall <- if (is.null(overall_test$p)) {
  pf(overall_test$F[2], overall_test$df[2], overall_test$ddf[2], lower.tail = FALSE)
} else overall_test$p[2]
```

# RCS的节点（knots）与“加权百分位数”的意义

- 节点数量：决定曲线的灵活度。RCS常用 3–5 个内节点；节点越多曲线越灵活，但方差也可能增大。
- 节点位置：常用（加权）分位数，如 10%、30%、70%、90%。
- 为什么“加权分位数”？
  - 复杂抽样样本并非简单随机抽样，样本分布未必代表总体。
  - 用 `survey::svyquantile` 根据设计对象中的抽样权重、分层、整群，估计总体分位数，让节点落在“总体”的密集区，而不是“样本”的偏倚密集区。
  - 这样能让样条的形状与推断更稳健，避免节点过度依赖某一抽样层的过度/不足抽样。

代码示例（加权分位数与RCS变量）

```r
# 设计对象（包含权重、分层、整群）
design <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR, nest = TRUE, data = sim_data)

# 加权分位数（返回矩阵，取第一列向量）
knots <- svyquantile(~TyG_WWI, design, quantiles = c(0.10, 0.30, 0.70, 0.90))[[1]][, 1]

# 预先在数据中创建RCS变换变量（与rms::rcs一致）
sim_data$TyG_WWI_rcs <- rcs(sim_data$TyG_WWI, knots)

# 重新创建设计对象以包含RCS变量
design <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR, nest = TRUE, data = sim_data)
```

自由度与解释要点

- 非线性检验的自由度（df1）约等于“样条基函数的个数 - 1”（因为线性成分不计入“非线性”部分）；整体效应检验的df1约等于“TyG_WWI相关全部项的个数”。`anova.svyglm`会返回适当的 `df`/`ddf`。
- 参照值（OR=1处）建议设在中位数附近，提升可读性与稳定性。
- 报告时建议同时给出：结点位置、结点数量、参照值、P for overall 与 P for non-linearity。

如何理解与使用

- 若 P for overall 显著而 P for non-linearity 不显著：可以考虑简化为线性模型。
- 若两者都显著：保留RCS并报告曲线形状与关键区间的OR。
- 若两者都不显著：在当前协变量与权重设定下，未观察到显著关联；可检查节点选择、样本量、事件率与权重稳定性。

常见坑与建议

- 使用 `scale_*` 的 `limits` 会丢弃超范围数据，导致曲线/带子被截断；推荐用 `coord_cartesian`。
- 节点放在极端尾部会导致外推不稳定、CI过宽；推荐用分位数节点。
- 事件率过低或权重方差过大，会使标准误增大（CI变宽）；可提高样本量、平滑生成机制或检查权重质量。
- 抽样设计一切从设计对象出发：任何统计量与分位数都应根据权重与分层计算。

结论

- P for non-linearity 检验“是否需要非线性”；P for overall 检验“是否有总体关联”。二者相辅相成：前者关注曲线形状，后者关注是否存在关系。
- 在复杂抽样数据中，用加权分位数设置RCS节点可以让模型更贴近总体、推断更稳健。
