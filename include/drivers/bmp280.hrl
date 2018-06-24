-define(ADDRESS, 16#76).

-define(REG_CONTROL, 16#F4).
-define(REG_RESULT_PRESSURE, 16#F7).     
-define(REG_RESULT_TEMPRERATURE, 16#FA).

-define(COMMAND_TEMPERATURE, 16#2E).
-define(COMMAND_PRESSURE0, 16#25).        
-define(COMMAND_PRESSURE1, 16#29).       
-define(COMMAND_PRESSURE2, 16#2D).    
-define(COMMAND_PRESSURE3, 16#31).    
-define(COMMAND_PRESSURE4, 16#5D).

-define(CAL_T, [
    {uint, 16#88},
    {int,  16#8A},
    {int,  16#8C}
  ]).
-define(CAL_P, [
    {uint, 16#8E},
    {int,  16#90},
    {int,  16#92},
    {int,  16#94},
    {int,  16#96},
    {int,  16#98},
    {int,  16#9A},
    {int,  16#9C},
    {int,  16#9E}
  ]).
