module PP where

import Types
import           Text.PrettyPrint
import           Prelude                 hiding ( (<>) )

--Utilities--
isBinary :: Form -> Bool
isBinary (And _ _)     = True
isBinary (Or _ _)      = True
isBinary (Implies _ _) = True
isBinary _             = False

parensIf :: Bool -> Doc -> Doc
parensIf True  d = parens d
parensIf False d = d

parensBinary :: Form -> Form -> String -> Doc
parensBinary p q op = parensIf (isBinary p) (ppForm p) <+> text op <+> 
                      parensIf (isBinary q) (ppForm q)
-------------

ppVar :: Var -> Doc
ppVar = text

ppForm :: Form -> Doc
ppForm FTrue         = text "T"
ppForm FBottom       = text "⊥"
ppForm (Var p)       = ppVar p
ppForm (Not p)       = text "¬" <> parensIf (isBinary p) (ppForm p)
ppForm (Box p)       = text "□" <> parensIf (isBinary p) (ppForm p)
ppForm (Dia p)       = text "◇" <> parensIf (isBinary p) (ppForm p)
ppForm (And p q)     = parensBinary p q "∧"
ppForm (Or p q)      = parensBinary p q "∨"
ppForm (Implies p q) = parensBinary p q "→"

renderForm :: Form -> String
renderForm = render . ppForm