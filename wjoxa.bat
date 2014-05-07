@echo off
echo.Starting Erlang Emulater.
echo.To start joxa shell in werl, run
echo.  joxa:main().
echo.To finish joxa shell, run
echo.  (require c)
echo.  (c/q)
start C:\erl6.0\bin\werl -pa .\deps\cucumberl\ebin -pa .\deps\erlware_commons\ebin -pa .\deps\getopt\ebin -pa .\deps\proper\ebin -pa .\deps\rebar_vsn_plugin\ebin -pa .\ebin
