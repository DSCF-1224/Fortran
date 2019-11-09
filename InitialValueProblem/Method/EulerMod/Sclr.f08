! repository   : Fortran / Initial Value Problem
! code created : 2019/11/07

dataBffr%step = dataCrnt%step
dataBffr%idvl = dataCrnt%idvl + val_step_idvl_half
dataBffr%sltn = dataCrnt%sltn + val_step_idvl_half * fncOde( dataCrnt )

dataNext%step = dataCrnt%step + 1
dataNext%idvl = dataCrnt%idvl + val_step_idvl
dataNext%sltn = dataCrnt%sltn + val_step_idvl * fncOde( dataBffr )

! --- EOF --- !
