--------------------------------------------------------------------------------
-- Copyright (c) 1995-2007 Xilinx, Inc.  All rights reserved.
--------------------------------------------------------------------------------
--   ____  ____
--  /   /\/   /
-- /___/  \  /    Vendor: Xilinx
-- \   \   \/     Version: J.31
--  \   \         Application: netgen
--  /   /         Filename: ic_74S181_sim.vhd
-- /___/   /\     Timestamp: Sun Mar 18 23:14:53 2007
-- \   \  /  \ 
--  \___\/\___\
--             
-- Command	: -intstyle ise -rpw 100 -tpw 0 -ar Structure -w -ofmt vhdl -sim ic_74S181.ngc ic_74S181_sim.vhd 
-- Device	: xc4vlx25-10-ff668
-- Input file	: ic_74S181.ngc
-- Output file	: ic_74S181_sim.vhd
-- # of Entities	: 1
-- Design Name	: ic_74S181
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

entity ic_74S181 is
  port (
    CIN_N : in STD_LOGIC := 'X'; 
    M : in STD_LOGIC := 'X'; 
    X : out STD_LOGIC; 
    Y : out STD_LOGIC; 
    COUT_N : out STD_LOGIC; 
    AEB : out STD_LOGIC; 
    F : out STD_LOGIC_VECTOR ( 3 downto 0 ); 
    A : in STD_LOGIC_VECTOR ( 3 downto 0 ); 
    B : in STD_LOGIC_VECTOR ( 3 downto 0 ); 
    S : in STD_LOGIC_VECTOR ( 3 downto 0 ) 
  );
end ic_74S181;

architecture Structure of ic_74S181 is
  signal Ckt74181_CLAmod3_Y_or0000_0 : STD_LOGIC; 
  signal N0 : STD_LOGIC; 
  signal N27 : STD_LOGIC; 
  signal N29 : STD_LOGIC; 
  signal N31 : STD_LOGIC; 
  signal N33 : STD_LOGIC; 
  signal N34 : STD_LOGIC; 
  signal N36 : STD_LOGIC; 
  signal N37 : STD_LOGIC; 
  signal NlwRenamedSig_OI_F : STD_LOGIC_VECTOR ( 3 downto 0 ); 
  signal Ckt74181_Summod4_CM : STD_LOGIC_VECTOR ( 1 downto 1 ); 
  signal Ckt74181_D : STD_LOGIC_VECTOR ( 3 downto 0 ); 
  signal Ckt74181_E : STD_LOGIC_VECTOR ( 3 downto 0 ); 
begin
  F(3) <= NlwRenamedSig_OI_F(3);
  F(2) <= NlwRenamedSig_OI_F(2);
  F(1) <= NlwRenamedSig_OI_F(1);
  F(0) <= NlwRenamedSig_OI_F(0);
  Ckt74181_Summod4_Mxor_F_1_Result1 : LUT3
    generic map(
      INIT => X"96"
    )
    port map (
      I0 => Ckt74181_D(1),
      I1 => Ckt74181_E(1),
      I2 => Ckt74181_Summod4_CM(1),
      O => NlwRenamedSig_OI_F(1)
    );
  Ckt74181_Summod4_CM_3_or000011 : LUT4
    generic map(
      INIT => X"15FF"
    )
    port map (
      I0 => Ckt74181_D(0),
      I1 => Ckt74181_E(0),
      I2 => CIN_N,
      I3 => Ckt74181_E(1),
      O => N0
    );
  Ckt74181_Summod4_CM_1_or00001 : LUT4
    generic map(
      INIT => X"FF15"
    )
    port map (
      I0 => Ckt74181_D(0),
      I1 => CIN_N,
      I2 => Ckt74181_E(0),
      I3 => M,
      O => Ckt74181_Summod4_CM(1)
    );
  Ckt74181_Summod4_AEB1 : LUT4
    generic map(
      INIT => X"8000"
    )
    port map (
      I0 => NlwRenamedSig_OI_F(1),
      I1 => NlwRenamedSig_OI_F(0),
      I2 => NlwRenamedSig_OI_F(3),
      I3 => NlwRenamedSig_OI_F(2),
      O => AEB
    );
  Ckt74181_CLAmod3_X1 : LUT4
    generic map(
      INIT => X"7FFF"
    )
    port map (
      I0 => Ckt74181_E(3),
      I1 => Ckt74181_E(2),
      I2 => Ckt74181_E(1),
      I3 => Ckt74181_E(0),
      O => X
    );
  Ckt74181_Emod1_E_3_not00001 : LUT4
    generic map(
      INIT => X"5D7F"
    )
    port map (
      I0 => A(3),
      I1 => B(3),
      I2 => S(3),
      I3 => S(2),
      O => Ckt74181_E(3)
    );
  Ckt74181_Emod1_E_2_not00001 : LUT4
    generic map(
      INIT => X"5D7F"
    )
    port map (
      I0 => A(2),
      I1 => B(2),
      I2 => S(3),
      I3 => S(2),
      O => Ckt74181_E(2)
    );
  Ckt74181_Emod1_E_1_not00001 : LUT4
    generic map(
      INIT => X"5D7F"
    )
    port map (
      I0 => A(1),
      I1 => B(1),
      I2 => S(3),
      I3 => S(2),
      O => Ckt74181_E(1)
    );
  Ckt74181_Emod1_E_0_not00001 : LUT4
    generic map(
      INIT => X"5D7F"
    )
    port map (
      I0 => A(0),
      I1 => B(0),
      I2 => S(3),
      I3 => S(2),
      O => Ckt74181_E(0)
    );
  Ckt74181_Dmod2_D_3_not00001 : LUT4
    generic map(
      INIT => X"0145"
    )
    port map (
      I0 => A(3),
      I1 => B(3),
      I2 => S(1),
      I3 => S(0),
      O => Ckt74181_D(3)
    );
  Ckt74181_Dmod2_D_2_not00001 : LUT4
    generic map(
      INIT => X"0145"
    )
    port map (
      I0 => A(2),
      I1 => B(2),
      I2 => S(1),
      I3 => S(0),
      O => Ckt74181_D(2)
    );
  Ckt74181_Dmod2_D_1_not00001 : LUT4
    generic map(
      INIT => X"0145"
    )
    port map (
      I0 => A(1),
      I1 => B(1),
      I2 => S(1),
      I3 => S(0),
      O => Ckt74181_D(1)
    );
  Ckt74181_Dmod2_D_0_not00001 : LUT4
    generic map(
      INIT => X"0145"
    )
    port map (
      I0 => A(0),
      I1 => B(0),
      I2 => S(1),
      I3 => S(0),
      O => Ckt74181_D(0)
    );
  Ckt74181_Summod4_Mxor_F_0_Result1 : LUT4
    generic map(
      INIT => X"D22D"
    )
    port map (
      I0 => CIN_N,
      I1 => M,
      I2 => Ckt74181_D(0),
      I3 => Ckt74181_E(0),
      O => NlwRenamedSig_OI_F(0)
    );
  Ckt74181_CLAmod3_CN4b_SW0 : LUT3
    generic map(
      INIT => X"80"
    )
    port map (
      I0 => Ckt74181_E(1),
      I1 => Ckt74181_E(0),
      I2 => CIN_N,
      O => N27
    );
  Ckt74181_CLAmod3_CN4b : LUT4
    generic map(
      INIT => X"FF80"
    )
    port map (
      I0 => Ckt74181_E(3),
      I1 => Ckt74181_E(2),
      I2 => N27,
      I3 => Ckt74181_CLAmod3_Y_or0000_0,
      O => COUT_N
    );
  Ckt74181_CLAmod3_Y_or0000_SW0 : LUT4
    generic map(
      INIT => X"AA80"
    )
    port map (
      I0 => Ckt74181_E(2),
      I1 => Ckt74181_E(1),
      I2 => Ckt74181_D(0),
      I3 => Ckt74181_D(1),
      O => N29
    );
  Ckt74181_CLAmod3_Y_or0000 : LUT4
    generic map(
      INIT => X"FFA8"
    )
    port map (
      I0 => Ckt74181_E(3),
      I1 => Ckt74181_D(2),
      I2 => N29,
      I3 => Ckt74181_D(3),
      O => Ckt74181_CLAmod3_Y_or0000_0
    );
  Ckt74181_Summod4_Mxor_F_3_Result_SW0 : LUT4
    generic map(
      INIT => X"0233"
    )
    port map (
      I0 => N0,
      I1 => Ckt74181_D(2),
      I2 => Ckt74181_D(1),
      I3 => Ckt74181_E(2),
      O => N31
    );
  Ckt74181_Summod4_Mxor_F_3_Result : LUT4
    generic map(
      INIT => X"C396"
    )
    port map (
      I0 => M,
      I1 => Ckt74181_E(3),
      I2 => Ckt74181_D(3),
      I3 => N31,
      O => NlwRenamedSig_OI_F(3)
    );
  Ckt74181_CLAmod3_Y1 : LUT4
    generic map(
      INIT => X"010F"
    )
    port map (
      I0 => N29,
      I1 => Ckt74181_D(2),
      I2 => Ckt74181_D(3),
      I3 => Ckt74181_E(3),
      O => Y
    );
  XST_VCC : VCC
    port map (
      P => N33
    );
  XST_GND : GND
    port map (
      G => N34
    );
  Ckt74181_Summod4_Mxor_F_2_Result11 : LUT4
    generic map(
      INIT => X"D22D"
    )
    port map (
      I0 => Ckt74181_D(1),
      I1 => M,
      I2 => Ckt74181_E(2),
      I3 => Ckt74181_D(2),
      O => N36
    );
  Ckt74181_Summod4_Mxor_F_2_Result12 : LUT3
    generic map(
      INIT => X"96"
    )
    port map (
      I0 => Ckt74181_D(2),
      I1 => Ckt74181_E(2),
      I2 => M,
      O => N37
    );
  Ckt74181_Summod4_Mxor_F_2_Result1_f5 : MUXF5
    port map (
      I0 => N37,
      I1 => N36,
      S => N0,
      O => NlwRenamedSig_OI_F(2)
    );

end Structure;

