library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

--  Uncomment the following lines to use the declarations that are
--  provided for instantiating Xilinx primitive components.
--library UNISIM;
--use UNISIM.VComponents.all;

entity cadr2_maskleft is
  port (mskl			: in std_logic_vector(4 downto 0);
  	    msk_left_out	: out std_logic_vector(31 downto 0));
end cadr2_maskleft;

architecture Behavioral of cadr2_maskleft is

begin
  with mskl select
  	msk_left_out <= X"00000001" when "00000",
			X"00000003" when "00001",
		   X"00000007" when "00010",
		   X"0000000f" when "00011",
		   X"0000001f" when "00100",
		   X"0000003f" when "00101",
		   X"0000007f" when "00110",
		   X"000000ff" when "00111",
		   X"000001ff" when "01000",
		   X"000003ff" when "01001",
		   X"000007ff" when "01010",
		   X"00000fff" when "01011",
		   X"00001fff" when "01100",
		   X"00003fff" when "01101",
		   X"00007fff" when "01110",
		   X"0000ffff" when "01111",
		   X"0001ffff" when "10000",
		   X"0003ffff" when "10001",
		   X"0007ffff" when "10010",
		   X"000fffff" when "10011",
		   X"001fffff" when "10100",
		   X"003fffff" when "10101",
		   X"007fffff" when "10110",
		   X"00ffffff" when "10111",
		   X"01ffffff" when "11000",
		   X"03ffffff" when "11001",
		   X"07ffffff" when "11010",
		   X"0fffffff" when "11011",
		   X"1fffffff" when "11100",
		   X"3fffffff" when "11101",
		   X"7fffffff" when "11110",
		   X"ffffffff" when "11111",
			X"00000000" when others;
end Behavioral;
