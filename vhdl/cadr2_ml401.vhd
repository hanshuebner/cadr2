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
-- Description:	Top level design for Xilinx ML401 dev board
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

use work.wb_pack.all;

entity cadr2_ml401 is
port (
	sys_clk_in	: in std_logic;
	sys_rst_in	: in std_logic
);
end cadr2_ml401;

architecture Behavioral of cadr2_ml401 is

component cadr2_cpu is
port (
	wb_out	: out wb_mem_out_type;
	wb_in		: in wb_mem_in_type;
	clk		: in std_logic;
	reset		: in std_logic
);
end component;

signal wb_out			: wb_mem_out_type;
signal wb_in			: wb_mem_in_type;

begin

cpu : cadr2_cpu port map(wb_out, wb_in, sys_clk_in, sys_rst_in);

end Behavioral;
