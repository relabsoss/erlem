-define(ADDRESS, 16#76).

-define(REG_CONTROL, 16#F4).
-define(REG_RESULT, 16#F7).     

-define(READ_CMD, 16#3F).

-define(MODE_ULTRALOWPOWER, 0).
-define(MODE_STANDART, 1).
-define(MODE_HIRES, 2).
-define(MODE_ULTRAHIRES, 3).

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
