! repository   : Fortran / Initial Value Problem
! code created : 2019/11/06

data_sltn_next%step = data_sltn_crnt%step + 1
data_sltn_next%idvl = data_sltn_crnt%step + val_step_idvl
data_sltn_next%sltn = data_sltn_crnt%sltn + val_step_idvl * func_eqn_diff( data_sltn_crnt )

! --- EOF --- !
