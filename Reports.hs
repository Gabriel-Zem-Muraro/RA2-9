module Reports where

import Types
import Data.List (sortBy, group, sort, isInfixOf, isPrefixOf, tails)
import Data.Ord (comparing, Down(..))
import qualified Data.Map as Map

historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem itemId logs = filter (\lg -> ("ID: " ++ itemId) `isInfixOf` (detalhes lg)) logs

-- Filtra logs que são falhas
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter isFalha
  where
    isFalha lg = case status lg of
      Falha _ -> True
      Sucesso -> False

-- Determina o item mais movimentado (retorna Maybe itemId)
itemMaisMovimentado :: [LogEntry] -> Maybe String
itemMaisMovimentado logs
  | null idsExtraidos = Nothing
  | null frequencias = Nothing
  | otherwise = Just $ fst $ head frequencias
  where
    logsRelevantes = filter isOperacaoItem logs
    isOperacaoItem lg = acao lg `elem` [Add, Remove, Update]
    idsExtraidos = concatMap extrairItemID logsRelevantes
    frequencias = sortBy (comparing (Down . snd))
                $ Map.toList
                $ Map.fromListWith (+)
                $ map (\i -> (i, 1 :: Int)) idsExtraidos

-- Extrai o ID do item a partir do campo 'detalhes' do log.
-- Procura explicitamente pela substring "ID:" e retorna o token seguinte
-- até encontrar um separador (espaço, vírgula, parêntese, ou ponto-e-vírgula).
extrairItemID :: LogEntry -> [String]
extrairItemID lg = case findId (detalhes lg) of
    Just s -> [s]
    Nothing -> []
  where
    idLabel = "ID:"
    separators = " ,);"
    -- procura recursivamente por "ID:" em toda string (caso haja outros ':')
    findId :: String -> Maybe String
    findId s
      | null s = Nothing
      | idLabel `isPrefixOf` s = extractAfterLabel (drop (length idLabel) s)
      | otherwise = findId (tail s)

    extractAfterLabel :: String -> Maybe String
    extractAfterLabel s =
      let after = dropWhile (== ' ') s
          idPart = takeWhile (`notElem` separators) after
      in if null idPart then Nothing else Just idPart

-- Gera string do relatório de erros
relatorioErros :: [LogEntry] -> String
relatorioErros logs
  | null erros = "Nenhum erro registrado no sistema."
  | otherwise = unlines $
      ["=== RELATÓRIO DE ERROS ===", ""] ++
      map formatarErro (zip [1..] erros) ++
      ["", "Total de erros: " ++ show (length erros)]
  where
    erros = logsDeErro logs

    formatarErro (n, lg) =
      show (n :: Int) ++ ". [" ++ show (acao lg) ++ "] " ++
      detalhes lg ++ "\n   Motivo: " ++ extrairMensagemErro (status lg)

    extrairMensagemErro (Falha msg) = msg
    extrairMensagemErro Sucesso = "N/A"

-- Relatório de histórico por item (string formatada)
relatorioHistoricoItem :: String -> [LogEntry] -> String
relatorioHistoricoItem itemId logs
  | null historico = "Nenhuma operação encontrada para o item '" ++ itemId ++ "'."
  | otherwise = unlines $
      ["=== HISTÓRICO DO ITEM: " ++ itemId ++ " ===", ""] ++
      map formatarLog (zip [1..] historico) ++
      ["", "Total de operações: " ++ show (length historico)]
  where
    historico = historicoPorItem itemId logs

    formatarLog (n, lg) =
      show (n :: Int) ++ ". [" ++ show (acao lg) ++ "] " ++
      show (timestamp lg) ++ "\n   " ++ detalhes lg ++
      "\n   Status: " ++ show (status lg)

-- Relatório do item mais movimentado (string)
relatorioItemMaisMovimentado :: [LogEntry] -> String
relatorioItemMaisMovimentado logs = case itemMaisMovimentado logs of
  Nothing -> "Nenhum item foi movimentado ainda."
  Just itemId ->
    "Item mais movimentado: " ++ itemId ++ "\n" ++
    "Total de operações: " ++ show (length $ historicoPorItem itemId logs)