#!/bin/sh

clisp -E ISO-8859-1 -x "(asdf:oos 'asdf:load-op :convert-man) (convert-man::produce-manual)"
