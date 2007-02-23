-- Module Name:    cadr2_dispatch - Behavioral
-- Project Name:   
-- Target Device:  
-- Tool versions:  
-- Description:
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

entity cadr2_dispatch is
  Port ( addr : in std_logic_vector(10 downto 0);
			din : in std_logic_vector(16 downto 0);
			dout : out std_logic_vector(16 downto 0);
			wr : in std_logic;
			en : in std_logic;
			clk	: in std_logic
);
end cadr2_dispatch;

architecture low_level of cadr2_dispatch is

begin

   RAMB16_S18_0 : RAMB16_S9
   generic map (
      INIT => X"00000", --  Value of output RAM registers at startup
      SRVAL => X"00000", --  Ouput value upon SSR assertion
      WRITE_MODE => "WRITE_FIRST" --  WRITE_FIRST, READ_FIRST or NO_CHANGE
      )
   port map (
      DO => dout(7 downto 0),      -- 8-bit Data Output
		DOP => dout(8 downto 8),
      ADDR => addr,  -- 11-bit Address Input
      CLK => clk,    -- Clock
      DI => din(7 downto 0),      -- 8-bit Data Input
		DIP => din(8 downto 8),
      EN => en,      -- RAM Enable Input
      SSR => '0',    -- Synchronous Set/Reset Input
      WE => wr       -- Write Enable Input
   );

   RAMB16_S18_1 : RAMB16_S9
   generic map (
      INIT => X"00000", --  Value of output RAM registers at startup
      SRVAL => X"00000", --  Ouput value upon SSR assertion
      WRITE_MODE => "WRITE_FIRST" --  WRITE_FIRST, READ_FIRST or NO_CHANGE
      )
   port map (
      DO => dout(16 downto 9),      -- 8-bit Data Output
      ADDR => addr,  -- 11-bit Address Input
      CLK => clk,    -- Clock
      DI => din(16 downto 9),      -- 8-bit Data Input
      EN => en,      -- RAM Enable Input
      SSR => '0',    -- Synchronous Set/Reset Input
      WE => wr       -- Write Enable Input
   );

end low_level;