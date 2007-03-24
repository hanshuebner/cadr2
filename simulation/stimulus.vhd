-----------------------------------------------------------------
--                                                             --
-----------------------------------------------------------------


LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
--USE IEEE.STD_LOGIC_ARITH.ALL;
USE STD.TEXTIO.ALL;

use work.wb_pack.all;

ENTITY STIMULUS IS
  PORT (
        CLK : out std_logic;
		  RST : out std_logic
  );
END STIMULUS;

ARCHITECTURE behav_arch OF STIMULUS is

CONSTANT TCLKH        : TIME := 15 NS;
CONSTANT TCLKL        : TIME := 15 NS;
CONSTANT TDEL         : TIME := 5 NS;

SIGNAL OPERATION      : STRING(1 TO 12) := (OTHERS => ' ');
SIGNAL STATUS_FOLLOW          : STRING(1 TO 8);
   
signal TCLK : std_logic;
signal TCLK_EN : std_logic;

signal REG_RST : std_logic := '0';

SIGNAL TEST_NO          : INTEGER;

  -- PROCEDURE TO PRINT MESSAGES TO STDOUT
  PROCEDURE PRINT(S: IN STRING) IS
    VARIABLE L: LINE;
  BEGIN
    WRITE(L, S);
    WRITELINE(OUTPUT,L);
  END PRINT;

BEGIN

  --  DEFINE PORT HOOKUP

  RST    <= REG_RST;

  -- CLOCK GENERATION

  PROCESS
  BEGIN
    TCLK <= '0';
    WAIT FOR TCLKL;
    TCLK <= TCLK_EN;
    WAIT FOR TCLKH;
  END PROCESS;

  CLK <= TCLK;


  -- THIS IS THE SIMULATION PROCESS.  BEGIN
  -- BY DEFINING SIMULATION PROCEDURES FOR
  -- THE SIMULATION SCRIPT.

  PROCESS

  BEGIN
   TEST_NO <= 0;
   
    -- START BY SETTING UP ALL SIGNALS
    OPERATION <= "SYSTEM RESET";
    TCLK_EN <= '1';
    REG_RST <= '1';

	 WAIT FOR 1 ns;
    -- RELEASE SYSTEM RESET
    REG_RST <= '0';
    PRINT(" ");
    PRINT("SYSTEM RESET COMPLETE...");
    PRINT(" ");
    
    WAIT FOR 5000 us;
   
    -- DISABLE CLK
    TCLK_EN <= '0';
 
    -- STOP SIMULATION
    ASSERT FALSE
    REPORT "SIMULATION COMPLETE (THIS IS NOT A FAILURE.)"
    SEVERITY FAILURE;

END PROCESS;

END BEHAV_ARCH;


