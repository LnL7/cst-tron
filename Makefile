debug: STRING_D.OBJ main.asm
	ml /Zi /W3 /WX STRING_D.OBJ main.asm /Fe DEBUG.EXE

cv: debug
	cv /s /43 DEBUG.EXE

STRING_D.OBJ: string.asm
	ml /c /Zi /W3 /WX /FoSTRING_D.OBJ /Ta string.asm

clean:
	del *.EXE
	del *.OBJ
