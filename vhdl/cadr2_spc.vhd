----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    18:45:21 07/29/2006 
-- Design Name: 
-- Module Name:    cadr2_spc - Behavioral 
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

entity cadr2_spc is
  Port ( addr : in std_logic_vector(4 downto 0);
			di : in std_logic_vector(18 downto 0);
			do : out std_logic_vector(18 downto 0);
			wr : in std_logic;
			wclk	: in std_logic
			);
end cadr2_spc;

architecture Behavioral of cadr2_spc is

begin

spc_loop: for i in 0 to 18 generate
begin
   RAM32X1S_inst : RAM32X1S
   generic map (
      INIT => X"00000000")
   port map (
      O => do(i),       -- RAM output
      A0 => addr(0),     -- RAM address[0] input
      A1 => addr(1),     -- RAM address[1] input
      A2 => addr(2),     -- RAM address[2] input
      A3 => addr(3),     -- RAM address[3] input
      A4 => addr(4),    -- RAM address[4] input
      D => di(i),      -- RAM data input
      WCLK => wclk, 		-- Write clock input
      WE => wr      		-- Write enable input
   );
end generate spc_loop;

end Behavioral;

