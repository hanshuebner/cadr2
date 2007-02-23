library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

--  Uncomment the following lines to use the declarations that are
--  provided for instantiating Xilinx primitive components.
--library UNISIM;
--use UNISIM.VComponents.all;

entity cadr2_maskright is
  port (mskr			: in std_logic_vector(4 downto 0);
  	    msk_right_out : out std_logic_vector(31 downto 0));
end cadr2_maskright;

architecture Behavioral of cadr2_maskright is

begin
  with mskr select
  	msk_right_out <= X"ffffffff" when "00000",
		   X"fffffffe" when "00001",
		   X"fffffffc" when "00010",
		   X"fffffff8" when "00011",
		   X"fffffff0" when "00100",
		   X"ffffffe0" when "00101",
		   X"ffffffc0" when "00110",
		   X"ffffff80" when "00111",
		   X"ffffff00" when "01000",
		   X"fffffe00" when "01001",
		   X"fffffc00" when "01010",
		   X"fffff800" when "01011",
		   X"fffff000" when "01100",
		   X"ffffe000" when "01101",
		   X"ffffc000" when "01110",
		   X"ffff8000" when "01111",
		   X"ffff0000" when "10000",
		   X"fffe0000" when "10001",
		   X"fffc0000" when "10010",
		   X"fff80000" when "10011",
		   X"fff00000" when "10100",
		   X"ffe00000" when "10101",
		   X"ffc00000" when "10110",
		   X"ff800000" when "10111",
		   X"ff000000" when "11000",
		   X"fe000000" when "11001",
		   X"fc000000" when "11010",
		   X"f8000000" when "11011",
		   X"f0000000" when "11100",
		   X"e0000000" when "11101",
		   X"c0000000" when "11110",
		   X"80000000" when "11111",
			X"00000000" when others;
end Behavioral;
