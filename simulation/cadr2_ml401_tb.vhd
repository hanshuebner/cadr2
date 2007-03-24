-------------------------------------------------------------------------------
-- cadr2_ml401_tb.vhd
-------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

library UNISIM;
use UNISIM.VCOMPONENTS.ALL;

use work.wb_pack.all;

entity cadr2_ml401_tb is
end cadr2_ml401_tb;

architecture STRUCTURE of cadr2_ml401_tb is

component cadr2_ml401 is
  port (
    sys_clk_in : in std_logic;
	 sys_rst_in : in std_logic
  );
end component cadr2_ml401;

-- stimulus file declaration
component STIMULUS is
  PORT (
        RST : out std_logic;
        CLK : out std_logic
  );
  end component;
  
signal RST : std_logic;
signal CLK : std_logic;

begin

-- PCI stimulus file    
  stimulus_i : STIMULUS
    port map (
			CLK => CLK,
			RST => RST
  );
  
system_i : cadr2_ml401
  port map (
		sys_clk_in => CLK,
		sys_rst_in => RST
  );

end architecture;
