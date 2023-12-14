<h4>Investigating, correcting and replying to errors</h4>

Please take a closer look at the do files where the columns <emph>Error</emph> or <emph>No data</emph> show a positive number. This is the number of Stata commands that have thrown an error. The column <emph>No data</emph> refers to errors that occured in cmds where previously a data set was not found, while the <emph>Error</emph> column refers to errors in commands where no data set was missing.

To investigate the errors, please click on the name of the corresponding code file in the first column. You will then see another HTML page that shows an interactive log file of your do code. Lines that throw errors have a grey background and there is a red comment before the first line in a block of error lines. You can press on the blue rectangle to the right to show the output of a command.

Please correct all errors you can correct. Afterward upload a corrected version of your supplement and let Codare analyse it again.

Some errors might be due to the fact that Codare uses a different version of an external Stata module than you use in your analysis. For example, newer [outreg2](http://repec.org/bocode/o/outreg2.html) versions, may not be compatible with code written for older versions. If you encounter such errors, please, directly add the ado files of the Stata modules that you used to your code supplement. Even if you don't encunter such errors, adding the ado files of the used Stata modules would generally be a good idea to improve reproducability of your supplement.

There can be errors, that you won't be able to correct. For example, the code may not run correctly because it requires confidential data that you cannot upload for the analysis on Codare. Some errors may also be due to Codare itself. To analyse your code, we augment the Stata code. In some cases, this code injection can induce errors. If such errors remain, please just briefly note it in your reply to the data editor.

