onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/clk
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/reset
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/spc
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/a
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/lc
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/destlc_n
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/ob
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/tse
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/alu
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/ib
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/lc_byte_mode
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/lcinc
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/ir
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/md
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/m
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/i
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/machrun
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/dc
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/mf
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/q
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/reta
add wave -noupdate -divider PC
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/pc
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/ipc
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/npc
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/opc
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/dpc
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/wpc
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/pcs1
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/pcs0
add wave -noupdate -divider {I RAM}
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/i_iram/addr
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/i_iram/di
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/i_iram/do
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/i_iram/wr
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/i_iram/en
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/i_iram/clk
add wave -noupdate -divider Dispatch
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/dadr
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/dwe
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/disp_dout
add wave -noupdate -divider {A & M mem}
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/aadr
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/amem
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/awp
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/madr
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/mmem
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/mwp
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/l
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/wadr
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/amemenb
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/apassenb
add wave -noupdate -divider PDL
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/pdl
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/pdla
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/pwp
add wave -noupdate -divider SPC
add wave -noupdate -format Logic /cadr2_ml401_tb/system_i/cpu/swp
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/spcptr
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/spcw
add wave -noupdate -format Literal /cadr2_ml401_tb/system_i/cpu/spco
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {28300 ps} 0}
configure wave -namecolwidth 261
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
update
WaveRestoreZoom {0 ps} {634880 ps}
