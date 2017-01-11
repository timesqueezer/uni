@echo off
java -cp bin.zip;scala-library.jar;scala-swing.jar;scala-parser-combinators.jar;rsyntaxtextarea.jar -Dswing.defaultlaf=com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel gui.Main
if %ERRORLEVEL%==0 goto end
echo.
echo Karel needs a Java runtime environment, because it is written in Scala.
echo Please download and install jre-8u60-windows-x64.exe from here and try again:
echo http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html
echo For very old 32 bit Windows systems, use jre-8u20-windows-i586.exe instead.
echo.
echo In case you have already installed Java and Karel still won't run,
echo make sure Java's bin directory is included in your PATH environment variable.
echo.
pause
:end
