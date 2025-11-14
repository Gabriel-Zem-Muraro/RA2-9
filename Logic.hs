module Logic where

import Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime)

addItem :: UTCTime -> String -> String -> Int -> String -> Inventario -> Either String ResultadoOperacao
addItem timestamp itemId itemNome qtd cat inv
  | qtd < 0 = Left "Erro: Quantidade não pode ser negativa"
  | Map.member itemId inv = Left $ "Erro: Item com ID '" ++ itemId ++ "' já existe no inventário"
  | null itemId = Left "Erro: ID do item não pode ser vazio"
  | null itemNome = Left "Erro: Nome do item não pode ser vazio"
  | null cat = Left "Erro: Categoria não pode ser vazia"
  | otherwise = Right (novoInv, logEntry)
  where
    novoItem = Item
      { itemID = itemId
      , nome = itemNome
      , quantidade = qtd
      , categoria = cat
      }
    novoInv = Map.insert itemId novoItem inv
    logEntry = LogEntry
      { timestamp = timestamp
      , acao = Add
      , detalhes = "Adicionado: " ++ itemNome ++ " (ID: " ++ itemId ++ 
                   ", Qtd: " ++ show qtd ++ ", Cat: " ++ cat ++ ")"
      , status = Sucesso
      }

removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem timestamp itemId qtdRemover inv
  | qtdRemover <= 0 = Left "Erro: Quantidade a remover deve ser maior que zero"
  | not (Map.member itemId inv) = Left $ "Erro: Item com ID '" ++ itemId ++ "' não encontrado"
  | quantidade item < qtdRemover = Left $ "Erro: Estoque insuficiente. Disponível: " ++ 
                                          show (quantidade item) ++ ", Solicitado: " ++ 
                                          show qtdRemover
  | otherwise = Right (novoInv, logEntry)
  where
    Just item = Map.lookup itemId inv
    novaQtd = quantidade item - qtdRemover
    itemAtualizado = item { quantidade = novaQtd }
    novoInv = if novaQtd == 0
              then Map.delete itemId inv
              else Map.insert itemId itemAtualizado inv
    logEntry = LogEntry
      { timestamp = timestamp
      , acao = Remove
      , detalhes = "Removido: " ++ nome item ++ " (ID: " ++ itemId ++ 
                   ", Qtd removida: " ++ show qtdRemover ++ 
                   ", Qtd restante: " ++ show novaQtd ++ ")"
      , status = Sucesso
      }

updateItem :: UTCTime -> String -> Maybe String -> Maybe Int -> Maybe String -> Inventario -> Either String ResultadoOperacao
updateItem timestamp itemId novoNome novaQtd novaCat inv
  | not (Map.member itemId inv) = Left $ "Erro: Item com ID '" ++ itemId ++ "' não encontrado"
  | Just qtd <- novaQtd, qtd < 0 = Left "Erro: Quantidade não pode ser negativa"
  | Just n <- novoNome, null n = Left "Erro: Nome não pode ser vazio"
  | Just c <- novaCat, null c = Left "Erro: Categoria não pode ser vazia"
  | novoNome == Nothing && novaQtd == Nothing && novaCat == Nothing = 
      Left "Erro: Nenhum campo foi especificado para atualização"
  | otherwise = Right (novoInv, logEntry)
  where
    Just itemOriginal = Map.lookup itemId inv
    itemAtualizado = Item
      { itemID = itemId
      , nome = maybe (nome itemOriginal) id novoNome
      , quantidade = maybe (quantidade itemOriginal) id novaQtd
      , categoria = maybe (categoria itemOriginal) id novaCat
      }
    novoInv = Map.insert itemId itemAtualizado inv
    mudancas = concat
      [ case novoNome of
          Just n -> "Nome: '" ++ nome itemOriginal ++ "' -> '" ++ n ++ "'; "
          Nothing -> ""
      , case novaQtd of
          Just q -> "Quantidade: " ++ show (quantidade itemOriginal) ++ " -> " ++ show q ++ "; "
          Nothing -> ""
      , case novaCat of
          Just c -> "Categoria: '" ++ categoria itemOriginal ++ "' -> '" ++ c ++ "'"
          Nothing -> ""
      ]
    
    logEntry = LogEntry
      { timestamp = timestamp
      , acao = Update
      , detalhes = "Atualizado item '" ++ nome itemAtualizado ++ "' (ID: " ++ itemId ++ "). " ++ mudancas
      , status = Sucesso
      }

updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty timestamp itemId novaQtd inv = 
  updateItem timestamp itemId Nothing (Just novaQtd) Nothing inv