Config {
  font = "xft:mono-11",
  bgColor = "#000000",
  fgColor = "#ffffff",

  -- Use either PC or laptop layout
  position = Static { xpos = 0 , ypos = 0, width = 1825, height = 22 },
  --  position = TopW L 95, -- leave 5% for the system tray

  commands = [
    -- Battery status indicator
    Run BatteryP ["BAT0"]
    ["-t", "<fc=yellow>B</fc>: <timeleft>h | <watts>",
     "-L", "10", "-H", "80", "-p", "3",
     "--", "-O", "<fc=green>On</fc> - ", "-o", "",
     "-L", "-15", "-H", "-5",
     "-l", "#FFB6B0", "-m", "#CEFFAC", "-h", "green"]
    10,

    -- Core Usage indicator
    Run MultiCpu [
      "-t", "<fc=yellow>C</fc>: <total>%",
      "-L","30","-H","60","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC","-w","3"]
    10,

    -- Memory Usage
    Run Memory [
      "-t","<fc=yellow>M: </fc> <usedratio>%",
      "-H","8192","-L","4096","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"]
    10,

    -- Swap Usage
    Run Swap
    ["-t","<fc=yellow>S</fc>: <usedratio>%",
     "-H","1024",
     "-L","512",
     "-h","#FFB6B0",
     "-l","#CEFFAC",
     "-n","#FFFFCC"] 10,

    -- Monitor network usage
    Run Network "eth0"
    ["-t","<fc=yellow>N: </fc> <rx>, <tx>",
     "-H","200",
     "-L","10",
     "-h","#FFB6B0",
     "-l","#CEFFAC",
     "-n","#FFFFCC"] 10,

    Run Network "wlan0"
    ["-t","<fc=yellow>N: </fc> <rx>, <tx>",
     "-H","200",
     "-L","10",
     "-h","#FFB6B0",
     "-l","#CEFFAC",
     "-n","#FFFFCC"] 10,

    -- Thermal zone
    Run ThermalZone 0 ["-t","<fc=yellow>T: </fc><temp>C"] 30,

    Run Date "%a %b %_d %l:%M" "date" 10,

    Run StdinReader
    ],
  sepChar = "%",
  alignSep = "}{",

  template = "%StdinReader% }{%thermal0% %battery% %wlan0% %eth0% %multicpu% %memory% %swap% <fc=#FFFFCC>%date%</fc> "
  }
