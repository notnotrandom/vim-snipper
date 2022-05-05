# Readme

Mostly tbd...

### Bugs

Say you have a snippet like so:

~~~
snippet e
	\emph{${1}}${2}
~~~

If you expand it, insert no text at the first tabstop, hit <Tab> to get to the second tabstop, hit <S-Tab> to back to the first tabstop, when you hit <Tab> again you will **not** go to the second tabstop. Rather you will stay put; hitting <Tab> again terminates the processing of the script, and lets you go back to typing your document.

As far as I can tell, this happens you have your snippet's last tabstop both a) ending a line and b) without a placeholder. This is so remote an use case that I can't really see myself expanding the time to fix it. Again as far as I can tell, shifting back and forth between tabstops seems to work properly for all other cases.
