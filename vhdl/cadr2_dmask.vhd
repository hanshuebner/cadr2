library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

--  Uncomment the following lines to use the declarations that are
--  provided for instantiating Xilinx primitive components.
--library UNISIM;
--use UNISIM.VComponents.all;

entity cadr2_dmask is
  port (addr : in std_logic_vector(4 downto 0);
  	    do : out std_logic_vector(6 downto 0));
end cadr2_dmask;

architecture Behavioral of cadr2_dmask is

begin
  with addr select
  	do <= "0000000" when "00000",
		   "0000001" when "00001",
		   "0000011" when "00010",
		   "0000111" when "00011",
		   "0001111" when "00100",
		   "0010000" when "00101",
		   "0111111" when "00110",
		   "1111111" when "00111",
		   "0000000" when others;
end Behavioral;
