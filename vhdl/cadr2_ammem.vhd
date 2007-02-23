--------------------------------------------------------------------------------
-- Company: 
-- Engineer:
--
-- Create Date:    14:50:58 08/07/06
-- Design Name:    cadr2
-- Module Name:    cadr2_ammem - Behavioral
-- Project Name:   
-- Target Device:  
-- Tool versions:  
-- Description:	Combined A and M memory.
--
-- Dependencies:
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
--------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

library UNISIM;
use UNISIM.VComponents.all;

entity cadr2_ammem is
  Port ( a_addr : in std_logic_vector(9 downto 0);
			a_di : in std_logic_vector(31 downto 0);
			a_do : out std_logic_vector(31 downto 0);
			a_wr : in std_logic;
			a_en : in std_logic;
			
			m_addr : in std_logic_vector(4 downto 0);
			m_di : in std_logic_vector(31 downto 0);
			m_do : out std_logic_vector(31 downto 0);
			m_wr : in std_logic;
			m_en : in std_logic;

			clk	: in std_logic
);
end cadr2_ammem;

architecture low_level of cadr2_ammem is

signal addrb : std_logic_vector(9 downto 0);
	
begin

   RAMB16_S18_S18_0 : RAMB16_S18_S18
   generic map (
      INIT_A => X"00000", --  Value of output RAM registers on Port A at startup
      INIT_B => X"00000", --  Value of output RAM registers on Port B at startup
      SRVAL_A => X"00000", --  Port A ouput value upon SSR assertion
      SRVAL_B => X"00000", --  Port B ouput value upon SSR assertion
      WRITE_MODE_A => "WRITE_FIRST", --  WRITE_FIRST, READ_FIRST or NO_CHANGE
      WRITE_MODE_B => "WRITE_FIRST", --  WRITE_FIRST, READ_FIRST or NO_CHANGE
      SIM_COLLISION_CHECK => "ALL" -- "NONE", "WARNING", "GENERATE_X_ONLY", "ALL
      )
   port map (
      DOA => a_do(15 downto 0),      -- Port A 16-bit Data Output
      DOB => m_do(15 downto 0),      -- Port B 16-bit Data Output
      ADDRA => a_addr,  -- Port A 10-bit Address Input
      ADDRB => addrb,  -- Port B 10-bit Address Input
      CLKA => clk,    -- Port A Clock
      CLKB => clk,    -- Port B Clock
      DIA => a_di(15 downto 0),      -- Port A 16-bit Data Input
      DIB => m_di(15 downto 0),      -- Port B 16-bit Data Input
      ENA => a_en,      -- Port A RAM Enable Input
      ENB => m_en,      -- PortB RAM Enable Input
      SSRA => '0',    -- Port A Synchronous Set/Reset Input
      SSRB => '0',    -- Port B Synchronous Set/Reset Input
      WEA => a_wr,      -- Port A Write Enable Input
      WEB => m_wr       -- Port B Write Enable Input
   );

   RAMB16_S18_S18_1 : RAMB16_S18_S18
   generic map (
      INIT_A => X"00000", --  Value of output RAM registers on Port A at startup
      INIT_B => X"00000", --  Value of output RAM registers on Port B at startup
      SRVAL_A => X"00000", --  Port A ouput value upon SSR assertion
      SRVAL_B => X"00000", --  Port B ouput value upon SSR assertion
      WRITE_MODE_A => "WRITE_FIRST", --  WRITE_FIRST, READ_FIRST or NO_CHANGE
      WRITE_MODE_B => "WRITE_FIRST", --  WRITE_FIRST, READ_FIRST or NO_CHANGE
      SIM_COLLISION_CHECK => "ALL" -- "NONE", "WARNING", "GENERATE_X_ONLY", "ALL
      )
   port map (
      DOA => a_do(31 downto 16),      -- Port A 16-bit Data Output
      DOB => m_do(31 downto 16),      -- Port B 16-bit Data Output
      ADDRA => a_addr,  -- Port A 10-bit Address Input
      ADDRB => addrb,  -- Port B 10-bit Address Input
      CLKA => clk,    -- Port A Clock
      CLKB => clk,    -- Port B Clock
      DIA => a_di(31 downto 16),      -- Port A 16-bit Data Input
      DIB => m_di(31 downto 16),      -- Port B 16-bit Data Input
      ENA => a_en,      -- Port A RAM Enable Input
      ENB => m_en,      -- PortB RAM Enable Input
      SSRA => '0',    -- Port A Synchronous Set/Reset Input
      SSRB => '0',    -- Port B Synchronous Set/Reset Input
      WEA => a_wr,      -- Port A Write Enable Input
      WEB => m_wr       -- Port B Write Enable Input
   );

addrb <= "00000" & m_addr;
	
end low_level;
