! repository   : Fortran / Initial Value Problem
! code created : 2019/11/06

dataNext%step = dataCrnt%step + 1
dataNext%idvl = dataCrnt%idvl + val_step_idvl
dataNext%sltn = dataCrnt%sltn + val_step_idvl * fncOde( dataCrnt )

! --- EOF --- !
