* Merge TokenMatcher and TokenExtractor
* Use line info from metada in error reporting
* Don't raise, better throw, and convert into a {:error, bla} return value for parse/1
* Make the upper behavior optiional, using a opt when "using" Grammar module
* Remove nasty IO.inspect() and IO.puts() :) 
