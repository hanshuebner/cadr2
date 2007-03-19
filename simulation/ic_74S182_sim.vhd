--------------------------------------------------------------------------------
-- Copyright (c) 1995-2007 Xilinx, Inc.  All rights reserved.
--------------------------------------------------------------------------------
--   ____  ____
--  /   /\/   /
-- /___/  \  /    Vendor: Xilinx
-- \   \   \/     Version: J.31
--  \   \         Application: netgen
--  /   /         Filename: ic_74S182_sim.vhd
-- /___/   /\     Timestamp: Sun Mar 18 23:15:03 2007
-- \   \  /  \ 
--  \___\/\___\
--             
-- Command	: -intstyle ise -rpw 100 -tpw 0 -ar Structure -w -ofmt vhdl -sim ic_74S182.ngc ic_74S182_sim.vhd 
-- Device	: xc4vlx25-10-ff668
-- Input file	: ic_74S182.ngc
-- Output file	: ic_74S182_sim.vhd
-- # of Entities	: 1
-- Design Name	: ic_74S182
-- Xilinx	: C:\Xilinx
--             
-- Purpose:    
--     This VHDL netlist is a verification model and uses simulation 
--     primitives which may not represent the true implementation of the 
--     device, however the netlist is functionally correct and should not 
--     be modified. This file cannot be synthesized and should only be used 
--     with supported simulation tools.
--             
-- Reference:  
--     Development System Reference Guide, Chapter 23
--     Synthesis and Simulation Design Guide, Chapter 6
--             
--------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
use UNISIM.VPKG.ALL;

entity ic_74S182 is
  port (
    COUT0_N : out STD_LOGIC; 
    CIN_N : in STD_LOGIC := 'X'; 
    COUT1_N : out STD_LOGIC; 
    COUT2_N : out STD_LOGIC; 
    YOUT : out STD_LOGIC; 
    XOUT : out STD_LOGIC; 
    X : in STD_LOGIC_VECTOR ( 3 downto 0 ); 
    Y : in STD_LOGIC_VECTOR ( 3 downto 0 ) 
  );
end ic_74S182;

architecture Structure of ic_74S182 is
  signal NlwRenamedSig_OI_COUT0_N : STD_LOGIC; 
  signal NlwRenamedSig_OI_COUT1_N : STD_LOGIC; 
  signal N7 : STD_LOGIC; 
  signal N9 : STD_LOGIC; 
  signal N10 : STD_LOGIC; 
begin
  COUT0_N <= NlwRenamedSig_OI_COUT0_N;
  COUT1_N <= NlwRenamedSig_OI_COUT1_N;
  Ckt74182_CNZ1 : LUT3
    generic map(
      INIT => X"4F"
    )
    port map (
      I0 => X(2),
      I1 => NlwRenamedSig_OI_COUT1_N,
      I2 => Y(2),
      O => COUT2_N
    );
  Ckt74182_CNY1 : LUT3
    generic map(
      INIT => X"4F"
    )
    port map (
      I0 => X(1),
      I1 => NlwRenamedSig_OI_COUT0_N,
      I2 => Y(1),
      O => NlwRenamedSig_OI_COUT1_N
    );
  Ckt74182_CNX1 : LUT3
    generic map(
      INIT => X"4F"
    )
    port map (
      I0 => X(0),
      I1 => CIN_N,
      I2 => Y(0),
      O => NlwRenamedSig_OI_COUT0_N
    );
  Ckt74182_PBo1 : LUT4
    generic map(
      INIT => X"FFFE"
    )
    port map (
      I0 => X(2),
      I1 => X(3),
      I2 => X(0),
      I3 => X(1),
      O => XOUT
    );
  Ckt74182_GBo_SW0 : LUT4
    generic map(
      INIT => X"EEEA"
    )
    port map (
      I0 => X(2),
      I1 => Y(1),
      I2 => Y(0),
      I3 => X(1),
      O => N7
    );
  Ckt74182_GBo : LUT4
    generic map(
      INIT => X"AA80"
    )
    port map (
      I0 => Y(3),
      I1 => Y(2),
      I2 => N7,
      I3 => X(3),
      O => YOUT
    );
  XST_VCC : VCC
    port map (
      P => N9
    );
  XST_GND : GND
    port map (
      G => N10
    );

end Structure;

