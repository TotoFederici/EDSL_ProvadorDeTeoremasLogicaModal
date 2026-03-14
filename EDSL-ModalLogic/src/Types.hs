module Types where

-- Variables proposicionales
type Var = String

-- Tipo para las formulas - AST
data Form = FTrue | FBottom
          | Var     String
          | Not     Form
          | And     Form Form
          | Or      Form Form
          | Implies Form Form
          | Dia     Form
          | Box     Form deriving (Show, Eq)

-- Un par (w,f) semanticamente significa que en el mundo w es valida la formula f
type World = Int
type LabeledForm = (World, Form)

-- Tablero que sirve como estado de la demostracion
data StateChart = SC [LabeledForm] -- Formulas Pendientes (por procesar)
                     [LabeledForm] -- Formulas Literales (Procesadas)
                     [Form]        -- Formulas Universales (Fromulas que valen en todos los mundos)
                     [World]       -- Lista de mundos 
                     World         -- Mundo mas grande creado
                     deriving Show