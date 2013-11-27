debug: SCREEN_D.OBJ STRING_D.OBJ main.asm
	ml /Zi /W3 /WX SCREEN_D.OBJ STRING_D.OBJ main.asm /Fe DEBUG.EXE

cv: debug
	cv /s /43 DEBUG.EXE

SCREEN_D.OBJ: screen.asm
	ml /c /Zi /W3 /WX /FoSCREEN_D.OBJ /Ta screen.asm
STRING_D.OBJ: string.asm
	ml /c /Zi /W3 /WX /FoSTRING_D.OBJ /Ta string.asm

clean:
	del *.EXE
	del *.OBJ
