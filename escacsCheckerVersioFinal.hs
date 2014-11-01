--Copyright (C) 2014 Martí Abril Montiel i Marc Fernández Vilchez

--Aquest programa és programari lliure, podeu redistribuir i / o modificar 
--sota els termes de la Llicència pública general de GNU com és publicada per 
--la Free Software Foundation, de la versió 2 de la Llicència, o 
--(A la seva elecció) qualsevol versió posterior. 

--Aquest programa es distribueix amb l'esperança que sigui útil, 
--però sense cap garantia, fins i tot sense la garantia implícita de 
--COMERCIALITZACIÓ o ADEQUACIÓ PER A PROPÒSITS DETERMINATS. Consulteu la 
--Llicència pública general de GNU per a més detalls. 

--Hi hauria d'haver rebut una còpia de la Llicència pública general de GNU 
--juntament amb aquest programa, si no, escriviu a la Free Software 
--Fundació, Inc, 59 Temple Place, Suite 330, Boston, MA 02111-1307 EUA

import System.IO 		-- llibreria bàsica per l'entrada sortida
import Data.Char 		-- llibreria necessària per dur a terme els castings de char a int
import Data.List 		-- llibreria necessària per executar funcions de llistes, com ara intersperse
import System.Directory	-- llibreria necessària per dur a terme les comprobacions de fitxer (exits..)

-- *************************************************************************************
-- *************************************************************************************
-- *************************** SECCIÓ MAIN + tractament fitxer *************************
-- *************************************************************************************
-- *************************************************************************************

main :: IO ()
main = do
		putStrLn "Donam un nom de fitxer a validar (o escriu \"s\" per tancar l'aplicatiu): "
		name <- getLine
		existeix <- doesFileExist name

		if name /= "s" then
			if existeix then do
				--let name = "partida_inmortal.txt"
				fitxer <- openFile  name ReadMode 		-- fitxer sera el fileHandler del fitxer name amb mode lectura		
				contingut <- hGetContents fitxer-- contingut sera l'String amb el contingut per llegir de fitxer
				mostrarTauler inicialitzarTauler
				tractamentFitxer 1 contingut inicialitzarTauler
				putStrLn "\n\n\t   ALGEBRAIC NOTATION CHESS CHECKER V.1"
				putStrLn "\tMartí Abril Montiel & Marc Fernández Vilchez\n\t\t\t   2014"
			else do 
				dirAct <- getCurrentDirectory
				putStrLn ("\nEl fitxer \"" ++ name ++ "\" no existeix... \n#Estàs en el director: " ++ dirAct ++ ")\n")
		else do
			putStrLn "\n\n\t   ALGEBRAIC NOTATION CHESS CHECKER V.1"
			putStrLn "\tMartí Abril Montiel & Marc Fernández Vilchez\n\t\t\t   2014"


tractamentFitxer num contingut tableruDeJuegu = do
	putStrLn ("\nTirada "++numJug ++"\n · Moviment jugador blanc: "++movBlanc++"\n · Moviment jugador negre: "++movNegre)
	--mostrarTauler tableruB
	putStrLn ("\n → Número de peces [InicialRonda: "++  show(obtNumPeces tableruDeJuegu) ++" B: " ++ show (obtNumPeces tableruB) ++ "; N: " ++ show (obtNumPeces tableruN)  ++ "]" )

	-- putStrLn ( " Num Peces assessines: " ++ show(length(obtPosAssessins tableruB Negre) )) -- ens mostra el numero de peces que ens estan fent escac		

	-- es comprova si hi ha hagut captura i de si s'ha indicat o no en el fitxer
	if obtNumPeces tableruDeJuegu /= obtNumPeces tableruB then do 
			putStrLn " → Hi ha hagut captura per part dels blancs" 
			if ((!!) movBlanc 3) == 'x' then putStr "" else error "«Captura no indicada en el fitxer...»"
	else if ((!!) movBlanc 3) == 'x' then error "«Blancs han indicat captura però no n'hi ha hagut...»" else putStr "" 
	if obtNumPeces tableruN /= obtNumPeces tableruB then do 
			putStrLn " → Hi ha hagut captura per part dels negres" 
			if ((!!) movNegre 3) == 'x' then putStr "" else error "«Captura no indicada en el fitxer...»"
	else if ((!!) movNegre 3) == 'x' then error "«Negres han indicat captura però no n'hi ha hagut...»" else putStr ""

	-- es comprova si hi ha hagut escac i mat, i de si s'ha indicat o no en el fitxer
	if escacMat tableruB Negre then do
		putStrLn " → Blancs han fet escac i mat al negres" 
		if ((!!) movBlanc ((length movBlanc)-2)) == '+' && (last movBlanc) == '+' then putStr "" else error "«Escac i Mat no indicat!!!»" 
	else if ((!!) movBlanc ((length movBlanc)-2)) == '+' && (last movBlanc) == '+' then error "«Blancs han indicat Escac i Mat però no n'hi ha hagut...»" else putStr ""
	if escacMat tableruN Blanc then do
		putStrLn " → Negres han fet escac i mat al Blancs" 
		if ((!!) movNegre ((length movNegre)-2)) == '+' && (last movNegre) == '+' then putStr "" else error "«Escac i Mat no indicat!!!»" 
	else if ((!!) movNegre ((length movNegre)-2)) == '+' && (last movNegre) == '+' then error "Negres han indicat Escac i Mat però no n'hi ha hagut...»" else putStr ""

	-- es comprova si hi ha hagut escac i de si s'ha indicat o no en el fitxer
	if escac tableruB Negre then do
		putStrLn " → Negres estan en escac!" 
		if ((last movBlanc) == '+') then putStr "" else error "«Blancs no han indicat que han fet escac als negres...»"
	else if ((last movBlanc) == '+')  then error "«Blancs han indicat erroneament que han fet escac als negres...»" else putStr "" 
	if escac tableruN Blanc then do
		putStrLn " → Blancs estan en escac!" 
		if ((last movNegre) == '+') then putStr "" else error "«Negres no han indicat que han fet escac als blancs...»"
	else if ((last movNegre) == '+')  then error "«Negres han indicat erroneament que han fet escac als blancs...»" else putStr "" 


	mostrarTauler tableruN -- mostrem els dos moviments de cop

	if resta/="" then tractamentFitxer (num+1) resta tableruN 
	else do
		putStrLn "\n\tFI DE PARTIDA!!!"
		if escacMat tableruN Negre then putStrLn "\tBlanques guanyen!!!"
		else if escacMat tableruN Blanc then putStrLn "\tNegres guanyen!!!" else putStrLn "\tEmpaaaaat!!!!"
		where 
			resta 		= tail (dropWhile (/='\n') contingut)
			tirada 		= words(takeWhile (/='\n') contingut)
			numJug 		= (!!) tirada 0
			movBlanc 	= (!!) tirada 1
			movNegre 	= if ((!!) movBlanc ((length movBlanc)-2)) == '+' && (last movBlanc) == '+' then "-----" else (!!) tirada 2
			tableruB 	= if escac auxJugadaB Blanc then error "\nMoviment BLANC invàlid, el rei queda en escac..." 
						  else auxJugadaB
			tableruN 	= if not(((!!) movBlanc ((length movBlanc)-2)) == '+' && (last movBlanc) == '+') then 
							if escac auxJugadaN Negre then error "\nMoviment NEGRE invàlid, el rei queda en escac..." 
							else auxJugadaN
						  else tableruB
			auxJugadaB  = fesJugada tableruDeJuegu (obtJugada movBlanc Blanc)
			auxJugadaN	= fesJugada tableruB (obtJugada movNegre Negre)

-- funció obtJugada que donada una string i un bàndol o Color retorna una Jugada fruit del desglossament de la String
obtJugada :: String -> Color -> Jugada
obtJugada s c 	= (peca, p1, p2)
	where 
		p 		= (!!) s 0
		p1		= obtPosDeString [((!!) s 1),((!!) s 2)]
		p2  	= if (((!!) s 3) /= 'x') then  obtPosDeString [((!!) s 3),((!!) s 4)] else obtPosDeString [((!!) s 4),((!!) s 5)]
		peca 	= if c == Blanc then p else toLower p


-- *************************************************************************************
-- *************************************************************************************
-- ******************************* SECCIÓ DECLARACIÓ TIPUS *****************************
-- *************************************************************************************
-- *************************************************************************************

-- tipus Posicio compost per Integer columna i Integer fila
--data Tauler = PP [(Peca, Posicio)] deriving Show
--type Posicio 	= (Int,Int)
data Posicio 	= Pos (Int, Int) deriving (Show, Eq) 
type Peca		= Char
type Tauler 	= [(Peca, Posicio)]
data Partida	= Par Tauler Bool deriving Show 
type Jugada 	= (Peca,Posicio,Posicio)
data Color		= Blanc | Negre deriving (Eq, Ord, Show)

-- *************************************************************************************
-- *************************************************************************************
-- *********************************** SECCIÓ OUTPUT ***********************************
-- *************************************************************************************
-- *************************************************************************************

-- funcio obtIconePeca que donat una peça retorna el seu caràcter ascii corresponent
obtIconePeca :: Peca -> Peca
obtIconePeca p 
	| p == 'p' 	= '♙'
	| p == 'P' 	= '♟'
	| p == 't' 	= '♖'
	| p == 'T' 	= '♜'
	| p == 'c' 	= '♘'
	| p == 'C' 	= '♞'
	| p == 'a' 	= '♗'
	| p == 'A' 	= '♝'	
	| p == 'd' 	= '♕'
	| p == 'D' 	= '♛'
	| p == 'r' 	= '♔'
	| p == 'R' 	= '♚'
	| otherwise = ' '

-- funcio que donat un tauler passat com a paràmetre el mostra per pantalla
mostrarTauler :: Tauler -> IO()
mostrarTauler t = putStrLn(	"    ╔═══════════════════════════════╗ " ++ map (obtIconePeca) ((obtPecesBandol inicialitzarTauler Blanc ) \\ (obtPecesBandol t Blanc)) ++ "\n" ++
							"  8 ║ " ++ intersperse ' ' (intersperse '│' [obtIconePeca(obtPecaDePosicio t (Pos(c,8))) | c <- [1..8]]) ++ " ║\n" ++
							"    ║–––┼–––┼–––┼–––┼–––┼–––┼–––┼–––║ \n" ++
							"  7 ║ " ++ intersperse ' ' (intersperse '│' [obtIconePeca(obtPecaDePosicio t (Pos(c,7))) | c <- [1..8]]) ++ " ║\n" ++
							"    ║–––┼–––┼–––┼–––┼–––┼–––┼–––┼–––║ \n" ++
							"  6 ║ " ++ intersperse ' ' (intersperse '│' [obtIconePeca(obtPecaDePosicio t (Pos(c,6))) | c <- [1..8]]) ++ " ║\n" ++
							"    ║–––┼–––┼–––┼–––┼–––┼–––┼–––┼–––║ \n" ++
							"  5 ║ " ++ intersperse ' ' (intersperse '│' [obtIconePeca(obtPecaDePosicio t (Pos(c,5))) | c <- [1..8]]) ++ " ║\n" ++
							"    ║–––┼–––┼–––┼–––┼–––┼–––┼–––┼–––║ \n" ++
							"  4 ║ " ++ intersperse ' ' (intersperse '│' [obtIconePeca(obtPecaDePosicio t (Pos(c,4))) | c <- [1..8]]) ++ " ║\n" ++
							"    ║–––┼–––┼–––┼–––┼–––┼–––┼–––┼–––║ \n" ++
							"  3 ║ " ++ intersperse ' ' (intersperse '│' [obtIconePeca(obtPecaDePosicio t (Pos(c,3))) | c <- [1..8]]) ++ " ║\n" ++
							"    ║–––┼–––┼–––┼–––┼–––┼–––┼–––┼–––║ \n" ++
							"  2 ║ " ++ intersperse ' ' (intersperse '│' [obtIconePeca(obtPecaDePosicio t (Pos(c,2))) | c <- [1..8]]) ++ " ║\n" ++
							"    ║–––┼–––┼–––┼–––┼–––┼–––┼–––┼–––║ \n" ++
							"  1 ║ " ++ intersperse ' ' (intersperse '│' [obtIconePeca(obtPecaDePosicio t (Pos(c,1))) | c <- [1..8]]) ++ " ║\n" ++
							"    ╚═══════════════════════════════╝ "++ map (obtIconePeca) ((obtPecesBandol inicialitzarTauler Negre ) \\ (obtPecesBandol t Negre)) ++ "\n" ++
							"      a   b   c   d   e   f   g   h") 


-- donat un tauler i una posició retorna la peça que conté
obtPecaDePosicio :: Tauler -> Posicio -> Peca
obtPecaDePosicio [] _ = '.'
obtPecaDePosicio ((pes,pos) : xs) posicio
	| pos == posicio 	= pes
	| otherwise 		= obtPecaDePosicio xs posicio


-- donat un tauler i una peça retorna la posició on es troba
obtPosicioDePeca :: Tauler -> Peca -> Posicio
obtPosicioDePeca [] _ = Pos(-1,-1)
obtPosicioDePeca ((pes,pos) : xs) peca
	| pes == peca 		= pos
	| otherwise 		= obtPosicioDePeca xs peca


-- *************************************************************************************
-- *************************************************************************************
-- ******************************** SECCIÓ CONTROL ESCAC *******************************
-- *************************************************************************************
-- *************************************************************************************

-- funció pucSalvarAlRei que donat un tauler, un bàndol i una llista de posicions a defensar, 
-- retorna cert si hi ha alguna peça en el tauler del mateix bàndol que pot assolir alguna 
-- de les posicions de la llista
pucSalvarAlRei :: Tauler -> Color -> [Posicio] -> Bool
pucSalvarAlRei _ _ [] 		= False
pucSalvarAlRei t col posicions = length([obtPecaDePosicio t (Pos(c, f)) | c<-[1..8], f<-[1..8], (intentarSalvarAlRei t col (obtPecaDePosicio t (Pos(c, f))) (Pos((c,f))) posicions) ]) /=0
	where
		esDelBandol = if col == Blanc then isUpper else isLower


-- funció intentarSalvarAlRei que rep un Tauler una Peca i una llista de Posicio, i retorna 
-- True si la peça pot assolir algunes de les posicions pasades amb un moviment legal
intentarSalvarAlRei :: Tauler -> Color -> Peca -> Posicio -> [Posicio] -> Bool
intentarSalvarAlRei _ _ _ _ [] 			= 	False
intentarSalvarAlRei t c p posPeca (pos : xs)	= 	if p /= '.' && (jugadaLegal t jugada) && color == c then 
												if not(escac (fesJugada t jugada) color) then 
												True else intentarSalvarAlRei t c p posPeca xs
											else intentarSalvarAlRei t c p posPeca xs
	where 
		jugada 	= (p,  posPeca, (pos))
		color 	= if isLower p then Negre else Blanc
	

-- funcio obtPosAdefensar que donat un tauler i dues posicions retorn una llista de les 
-- posicions que hi han entre mig, inclosa la segona posició
obtPosAdefensar :: Tauler -> Posicio -> Posicio -> [Posicio]
obtPosAdefensar t (Pos(c1,f1)) (Pos(c2,f2))
	| c1 == c2 				= [(Pos(c1,f)) | f <- [(fMin+1)..(fMax-1)]] ++ [(Pos(c2,f2))] 
	| f1 == f2 				= [(Pos(c,f1)) | c <- [(cMin+1)..(cMax-1)]] ++ [(Pos(c2,f2))]
	| f1 > f2 && c1 < c2	= [(Pos(c1+i,f1-i)) | i <- [1..8], (cMin+i) < cMax] ++ [(Pos(c2,f2))]
	| f1 < f2 && c1 > c2	= [(Pos(c1-i,f1+i)) | i <- [1..8], (cMin+i) < cMax] ++ [(Pos(c2,f2))]
	| otherwise				= [(Pos(cMin+i,fMin+i)) | i <- [1..8], (cMin+i) < cMax] ++ [(Pos(c2,f2))]
		where 	
			cMax = max c1 c2
			cMin = min c1 c2
			fMax = max f1 f2 
			fMin = min f1 f2

-- retorna un llistat de posicions on es troben les peces enemigues que ens estan fent escac
obtPosAssessins :: Tauler -> Color -> [Posicio]
obtPosAssessins [] _ 	= []
obtPosAssessins t c 	= 	hiHaEnemic t (atacPeo posRei c) peo ++
							(hiHaEnemic t (movimentCavall posRei) cavall) ++ 							
						    filtratgeEscacMat t (hiHaEnemic t (movimentDama posRei) dama) posRei ++
							filtratgeEscacMat t (hiHaEnemic t (movimentTorre posRei) torre) posRei ++
							filtratgeEscacMat t (hiHaEnemic t (movimentAlfil posRei) alfil) posRei							
	where 
		posRei 	= if c == Blanc then obtPosicioDePeca t 'R' else obtPosicioDePeca t 'r'
		torre 	= if c == Blanc then 't' else 'T'
		cavall 	= if c == Blanc then 'c' else 'C'
		alfil 	= if c == Blanc then 'a' else 'A'
		dama 	= if c == Blanc then 'd' else 'D'
		peo 	= if c == Blanc then 'p' else 'P'

-- funció escac que donat un Tauler i un bàndol (o color) ens digui si el rei
-- d'aquell bàndol està amenaçat.
escac :: Tauler -> Color -> Bool
escac t c = length((hiHaEnemic t (movimentCavall posRei) cavall)) > 0 			 ||
			filtratgeEscac t (hiHaEnemic t (movimentDama posRei) dama) posRei	 ||
			filtratgeEscac t (hiHaEnemic t (movimentTorre posRei) torre) posRei  ||
			filtratgeEscac t (hiHaEnemic t (movimentAlfil posRei) alfil) posRei  ||
			length(hiHaEnemic t (atacPeo posRei c) peo) > 0
	where 
		posRei 	= if c == Blanc then obtPosicioDePeca t 'R' else obtPosicioDePeca t 'r'
		torre 	= if c == Blanc then 't' else 'T'
		cavall 	= if c == Blanc then 'c' else 'C'
		alfil 	= if c == Blanc then 'a' else 'A'
		dama 	= if c == Blanc then 'd' else 'D'
		peo 	= if c == Blanc then 'p' else 'P'

-- funció escacMat que donat un Tauler i un bàndol (o color) ens digui si el rei
-- d'aquell bàndol està en escac i no en pot sortir (escac i mat).
-- [1. Està en escac; 2. El rei no es pot moure sense deixar d'estar en escac; 3. Ningú no el pot protegir]
escacMat :: Tauler -> Color -> Bool
escacMat t c = 	(escac t c) && 
				not(potMoures t c (movimentsRei t c)) && 
				not(pucSalvarAlRei t c ( obtPosAdefensar t posRei (head(obtPosAssessins t c))))
	where 
		posRei 	= if c == Blanc then obtPosicioDePeca t 'R' else obtPosicioDePeca t 'r'

-- funcio movimentsRei que donat un Tauler i un Color retorna una llista amb les posicions
-- que el Rei d'aquell bandol podria fer (sense tenir en compte si hi ha o no algú)
movimentsRei :: Tauler -> Color -> [Posicio]
movimentsRei t c 	= filter jugadaPosible (movimentRei posRei)
	 where 
	 	rei 						= if c == Blanc then 'R' else 'r'
	 	posRei 						= obtPosicioDePeca t rei
	 	jugadaPosible (Pos (c,f))	= jugadaLegal t (rei, posRei, (Pos(c,f)))

-- funcio potMoures que donat un tauler, un bandol i una llista de pocicions, retornarà cert
-- si el Rei es pot moure en alguna d'aquelles posicions sense estar en escac
potMoures :: Tauler -> Color -> [Posicio] -> Bool
potMoures _ _ []		= False
potMoures t c (p : xs)  = if not(escac (fesJugada t (rei, posRei, p)) c) then True else potMoures t c xs
	where 
		rei 			= if c == Blanc then 'R' else 'r'
	 	posRei 			= obtPosicioDePeca t rei


--retorna les posicions possibles on un peo ens podria atacar
atacPeo :: Posicio -> Color -> [Posicio]
atacPeo (Pos (c,f)) color
	| color == Blanc = filter dinsTauler [Pos (c+1,f+1), Pos (c-1,f+1)] 
	| otherwise 	 = filter dinsTauler [Pos (c-1,f-1), Pos (c+1,f-1)] 
 where dinsTauler (Pos (c,f)) = c `elem` [1..8] && f `elem` [1..8]

-- retorna True si hi ha algú entre la posicio i cadascuna de les posicions de la llista
filtratgeEscac :: Tauler -> [Posicio] -> Posicio -> Bool
filtratgeEscac _ [] _	 		= False
filtratgeEscac t (p : xs) pos 	= if not(alguEntre t p pos) then True else (filtratgeEscac t xs pos)


-- retorna una llista de posicions on hi ha peces enemigues que ataquen al rei
filtratgeEscacMat :: Tauler -> [Posicio] -> Posicio -> [Posicio]
filtratgeEscacMat _ [] _	 		= []
filtratgeEscacMat t (p : xs) pos 	= if not(alguEntre t p pos) then [p] ++ (filtratgeEscacMat t xs pos) else (filtratgeEscacMat t xs pos)

-- funcio que donada una llista de posicions i una peça, retorna una llista
-- de posicions on es troba l'enemic Peca
hiHaEnemic :: Tauler -> [Posicio] -> Peca -> [Posicio]
hiHaEnemic _ [] _ = []
hiHaEnemic t (p : xs) peca
	| (obtPecaDePosicio t p) == peca = [p] ++ (hiHaEnemic t xs peca)
	| otherwise = hiHaEnemic t xs peca

 
-- *************************************************************************************
-- *************************************************************************************
-- *********************************** SECCIÓ MOVIMENTS ********************************
-- *************************************************************************************
-- *************************************************************************************

-- donada una Peca i una posició, quines serien totes les posicions on podria anar en un tauler buit.
moviment :: Peca -> Posicio -> [Posicio]
moviment p pos
	| p == 'P' 				= movimentPeoBlanc pos
	| p == 'p' 				= movimentPeoNegre pos
	| p == 'T' || p == 't'	= movimentTorre pos
	| p == 'A' || p == 'a'	= movimentAlfil pos
	| p == 'C' || p == 'c'	= movimentCavall pos
	| p == 'D' || p == 'd'	= movimentDama pos
	| p == 'R' || p == 'r'	= movimentRei pos
	| p == '.'				= []
	| otherwise = error ("ERROR en el format del fitxer (" ++ [p] ++ ")")

-- funció alguEntre que donat un Tauler i dues posicions, ens digui si hi ha algú
-- entre les dues posicions (bàsicament per a desplaçaments d'`alfils, torres, reines 
-- i peons en sortida llarga).
alguEntre :: Tauler -> Posicio -> Posicio -> Bool
alguEntre t (Pos(c1,f1)) (Pos(c2,f2))
	| c1 == c2 				= length(filter (/='.') [obtPecaDePosicio t (Pos(c1,f)) | f <- [(fMin+1)..(fMax-1)]]) > 0
	| f1 == f2 				= length(filter (/='.') [obtPecaDePosicio t (Pos(c,f1)) | c <- [(cMin+1)..(cMax-1)]]) > 0
	| f1 > f2 && c1 < c2	= length(filter (/='.') [obtPecaDePosicio t (Pos(c1+i,f1-i)) | i <- [1..8], (cMin+i) < cMax]) > 0 
	| f1 < f2 && c1 > c2	= length(filter (/='.') [obtPecaDePosicio t (Pos(c1-i,f1+i)) | i <- [1..8], (cMin+i) < cMax]) > 0
	| otherwise				= length(filter (/='.') [obtPecaDePosicio t (Pos(cMin+i,fMin+i)) | i <- [1..8], (cMin+i) < cMax]) > 0
		where 	
			cMax = max c1 c2
			cMin = min c1 c2
			fMax = max f1 f2 
			fMin = min f1 f2


-- jugadaLegal que certifiqui que la Jugada proposada és legal.
--(el cavall és un cas especial ja que no s'ha de mirar si hi ha algú entre)
jugadaLegal :: Tauler -> Jugada -> Bool
jugadaLegal t (p,Pos(c1,f1),Pos(c2,f2))
	| p == 'P'			= if c1 == c2 then length(filter (==p2) (moviment p p1)) == 1 && pecaP2 == '.'
						  else length(filter (==p2) (atacPeo p1 Blanc)) == 1 && isLower pecaP2
	| p == 'p'			= if c1 == c2 then length(filter (==p2) (moviment p p1)) == 1 && pecaP2 == '.'
						  else length(filter (==p2) (atacPeo p1 Negre)) == 1 && isUpper pecaP2
	| p == 'C'  		= length(filter (==p2) (moviment p p1)) == 1 && (pecaP2 == '.' || isLower pecaP2)
	| p == 'c'  		= length(filter (==p2) (moviment p p1)) == 1 && (pecaP2 == '.' || isUpper pecaP2)
	| isLower p 		= length(filter (==p2) (moviment p p1)) == 1 && not (alguEntre t p1 p2) && (pecaP2 == '.' || isUpper pecaP2)
	| otherwise 		= length(filter (==p2) (moviment p p1)) == 1 && not (alguEntre t p1 p2) && (pecaP2 == '.' || isLower pecaP2)
	  where 
	  	pecaP2 		= (obtPecaDePosicio t p2)
	  	p1			= Pos(c1,f1)
	  	p2 			= Pos(c2,f2)


-- donat un Tauler i una Jugada ens torni un nou Tauler amb la jugada feta.
fesJugada :: Tauler -> Jugada -> Tauler
fesJugada t (p,p1,p2) = if (jugadaLegal t (p,p1,p2)) then 
							[ ((obtPecaDePosicio t (Pos(c, f))),(Pos(c, f))) | c<-[1..8], f<-[1..8], (Pos(c, f)) /= p1 && (Pos(c, f)) /= p2  ] ++ [('.',p1),(p,p2)] 
						else error ("Jugada dels " ++ color ++ "invàlida")
	where color = if isLower p then "negres" else "blancs"


--moviment del cavall sense tenir en compte si la posició està o no ocupada
movimentCavall :: Posicio -> [Posicio]
movimentCavall (Pos (c,f)) = filter dinsTauler [Pos (c+2,f-1),Pos (c+2,f+1),Pos (c-2,f-1),Pos (c-2,f+1),Pos (c+1,f-2),Pos (c+1,f+2),Pos (c-1,f-2),Pos (c-1,f+2)] 
 where dinsTauler (Pos (c,f)) = c `elem` [1..8] && f `elem` [1..8]

--moviment del Rei sense tenir en compte si la posició està o no ocupada
movimentRei :: Posicio -> [Posicio]
movimentRei (Pos (c,f)) = filter dinsTauler [Pos (c+1,f), Pos (c-1,f), Pos (c,f-1), Pos (c,f+1), Pos (c+1,f+1), Pos (c+1,f-1), Pos (c-1,f-1), Pos (c-1,f+1)] 
 where dinsTauler (Pos (c,f)) = c `elem` [1..8] && f `elem` [1..8]

--moviment de la torre sense tenir en compte si la posició està o no ocupada
movimentTorre :: Posicio -> [Posicio]
movimentTorre (Pos (c,f)) = [Pos (c,y) | y<-[1..8],y/=f] ++ [Pos (x,f) | x<-[1..8],x/=c]

--moviment de l'àfil sense tenir en compte si la posició està o no ocupada
movimentAlfil :: Posicio -> [Posicio]
movimentAlfil (Pos (c,f)) = [Pos (c+y,f+y) | y<-[1..8], (c+y < 9 && f+y < 9)] ++
							[Pos (c-y,f-y) | y<-[1..8], (c-y > 0 && f-y > 0)] ++
							[Pos (c+y,f-y) | y<-[1..8], (c+y < 9 && f-y > 0)] ++
							[Pos (c-y,f+y) | y<-[1..8], (c-y > 0 && f+y < 9)]

--moviment de la dama sense tenir en compte si la posició està o no ocupada
movimentDama :: Posicio -> [Posicio]
movimentDama pos = (movimentTorre pos) ++ (movimentAlfil pos)

--moviment del peó blanc sense tenir en compte si la posició està o no ocupada
movimentPeoBlanc :: Posicio -> [Posicio]
movimentPeoBlanc (Pos (c, f))
	| f == 2 	= [Pos (c,3),Pos (c,4)]
	| f < 8 	= [Pos (c,f+1)]
	| otherwise = []

--moviment del peó negre sense tenir en compte si la posició està o no ocupada
movimentPeoNegre :: Posicio -> [Posicio]
movimentPeoNegre (Pos (c, f))
	| f == 7 	= [Pos (c,6),Pos (c,5)]
	| f > 1 	= [Pos (c,f-1)]
	| otherwise = []

-- *************************************************************************************
-- *************************************************************************************
-- *********************************** SECCIÓ miscellaneous ****************************
-- *************************************************************************************
-- *************************************************************************************

-- donada les coordenades d'una posicio en format String, retorna la posicio corresponent
obtPosDeString :: String -> Posicio
obtPosDeString a = Pos(ord (head a) - 96, digitToInt (head (tail a)))


-- obtNumPeces que donat un tauler retorna un enter indicant el número de peces que conté
obtNumPeces :: Tauler -> Int
obtNumPeces [] 				= 0
obtNumPeces ((p,pos) : xs) 	= if (p /= '.') then (obtNumPeces xs)+1 else (obtNumPeces xs)+0

-- retorna el llistat de posicions on hi ha la peca
obtPeca :: Tauler -> Peca -> [Posicio]
obtPeca [] _ 	= []
obtPeca ((p, pos):xs) peca  =  if p == peca then [pos] ++ obtPeca xs peca else obtPeca xs peca

-- donat un tauler i un bàndol, retorna un llistat amb les peces que conté el tauler d'aquell bàndol
obtPecesBandol :: Tauler -> Color -> [Peca]
obtPecesBandol t col = filter bandol [obtPecaDePosicio t (Pos(c, f)) | c<-[1..8], f<-[1..8]]
	where
		bandol 		 = if col == Blanc then isUpper else isLower

-- inicialitza el tauler de joc
inicialitzarTauler :: Tauler
inicialitzarTauler 	= 	[('P',Pos(c, 2)) | c<-[1..8]] ++  		-- inicialització peons blancs
						[('T',Pos(1, 1)),('T',Pos(8, 1))] ++	-- inicialització torres blanques
						[('C',Pos(2, 1)),('C',Pos(7, 1))] ++	-- inicialització cavalls blancs
						[('A',Pos(3, 1)),('A',Pos(6, 1))] ++	-- inicialització àlfils blancs
						[('D',Pos(4, 1)),('R',Pos(5, 1))] ++ 	-- inicialització de la dama i rei blanc
						[('p',Pos(c, 7)) | c<-[1..8]] ++  		-- inicialització peons negres
						[('t',Pos(1, 8)),('t',Pos(8, 8))] ++	-- inicialització torres negres
						[('c',Pos(2, 8)),('c',Pos(7, 8))] ++	-- inicialització cavalls negres
						[('a',Pos(3, 8)),('a',Pos(6, 8))] ++	-- inicialització àlfils negres
						[('d',Pos(4, 8)),('r',Pos(5, 8))]		-- inicialització de la dama i rei negres
