--------------------------------------------------------------------------------
-- Company: 
-- Engineer:
--
-- Create Date:    14:50:58 11/11/05
-- Design Name:    
-- Module Name:    cadr2_ml401 - Behavioral
-- Project Name:   cadr2
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

---- Uncomment the following library declaration if instantiating
---- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity cadr2_ml401 is
port (
	ddr_data : 

	clk	: in std_logic;
	reset	: in std_logic
);
end cadr2_ml401;

architecture Behavioral of cadr2_ml401 is

component cadr2_cpu is
port (
	clk	: in std_logic;
	reset	: in std_logic
);
end component;

begin

cpu : cadr2_cpu port map(clk, reset);

end Behavioral;
