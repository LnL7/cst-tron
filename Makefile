debug: GAME_D.OBJ SCREEN_D.OBJ main.asm
	ml /Zi /W3 /WX GAME_D.OBJ SCREEN_D.OBJ main.asm /Fe DEBUG.EXE

cv: debug
	cv /s /43 DEBUG.EXE

GAME_D.OBJ: game.asm
	ml /c /Zi /W3 /WX /FoGAME_D.OBJ /Ta game.asm
SCREEN_D.OBJ: screen.asm
	ml /c /Zi /W3 /WX /FoSCREEN_D.OBJ /Ta screen.asm

clean:
	del *.EXE
	del *.OBJ
