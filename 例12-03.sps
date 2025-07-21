SPLIT FILE OFF.
COMPUTE t=(t0 + t1 + t2 + t3 + t4)/5.
EXECUTE.
MEANS TABLES=t by group.

GLM t0 t1 t2 t3 t4 BY group
  /WSFACTOR=factor1 5 Polynomial 
  /METHOD=SSTYPE(3)
  /POSTHOC=group(LSD) 
  /PLOT=PROFILE(factor1*group)
  /CRITERIA=ALPHA(.05)
  /WSDESIGN= factor1
  /DESIGN= group.

USE ALL.
COMPUTE filter_$=(group = 3).
VARIABLE LABEL filter_$ 'group = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMAT filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

GLM t0 t1 t2 t3 t4
  /WSFACTOR=factor1 5 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(factor1)
  /EMMEANS=TABLES(factor1) 
  /PRINT=DESCRIPTIVE 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN= factor1.

USE ALL.
COMPUTE filter_$=(group = 2).
VARIABLE LABEL filter_$ 'group = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMAT filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
T-TEST PAIRS=t1 WITH t0 (PAIRED).
T-TEST PAIRS=t2 WITH t0 (PAIRED).
T-TEST PAIRS=t3 WITH t0 (PAIRED).
T-TEST PAIRS=t4 WITH t0 (PAIRED).

COMPUTE t=(t0 + t1 + t2 + t3 + t4)/5.
EXECUTE.
