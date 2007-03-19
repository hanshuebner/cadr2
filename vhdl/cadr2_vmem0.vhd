----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    15:28:49 09/11/2006 
-- Design Name: 
-- Module Name:    cadr2_mmu - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

library UNISIM;
use UNISIM.VComponents.all;

entity cadr2_vmem0 is
    Port (
			addr : in std_logic_vector(10 downto 0);
			di : in std_logic_vector(4 downto 0);
			do : out std_logic_vector(4 downto 0);
			wr : in std_logic;
			en : in std_logic;
			clk : in std_logic);
end cadr2_vmem0;

architecture Behavioral of cadr2_vmem0 is

signal do_tmp : std_logic_vector(7 downto 0);
signal di_tmp : std_logic_vector(7 downto 0);
signal dip : std_logic_vector(0 downto 0);

begin

   mmu_l1 : RAMB16_S9
   generic map (
      INIT => X"000", --  Value of output RAM registers at startup
      SRVAL => X"000", --  Ouput value upon SSR assertion
      WRITE_MODE => "WRITE_FIRST") --  WRITE_FIRST, READ_FIRST or NO_CHANGE
   port map (
      DO => do_tmp,      -- 8-bit Data Output
      ADDR => addr,  -- 11-bit Address Input
      CLK => clk,    -- Clock
      DI => di_tmp,      -- 8-bit Data Input
		DIP => dip,
      EN => en,      -- RAM Enable Input
      SSR => '0',    -- Synchronous Set/Reset Input
      WE => wr       -- Write Enable Input
   );

di_tmp <= "000" & di;
do <= do_tmp(4 downto 0);
dip <= "0";

end Behavioral;

