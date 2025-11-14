module Main where

import Types
import Logic
import Reports
import qualified Data.Map as Map
import Data.Time (getCurrentTime, UTCTime)
import System.IO (hFlush, stdout, withFile, IOMode(ReadMode), hGetContents)
import Control.Exception (catch, IOException)
import System.Directory (doesFileExist)

arquivoInventario :: FilePath
arquivoInventario = "Inventario.dat"

arquivoAuditoria :: FilePath
arquivoAuditoria = "Auditoria.log"

main :: IO ()
main = do
  putStrLn "=== SISTEMA DE INVENTÁRIO ==="
  putStrLn "Inicializando sistema..."
  putStrLn ""
  
  arquivoExiste <- doesFileExist arquivoInventario
  
  invInicial <- if not arquivoExiste
                then do
                  putStrLn " Primeira execução detectada!"
                  popularDadosIniciais
                else do
                  inv <- carregarInventario
                  if Map.null inv
                    then popularDadosIniciais
                    else return inv
  
  logs <- carregarLogs
  loopPrincipal invInicial logs

carregarInventario :: IO Inventario
carregarInventario = do
  existe <- doesFileExist arquivoInventario
  if existe
    then (do
      conteudo <- readFile arquivoInventario
      let inv = read conteudo :: Inventario
      putStrLn $ " Inventário carregado: " ++ show (Map.size inv) ++ " itens"
      return inv)
    `catch`
      (\e -> do
        putStrLn $ "Erro ao ler inventário: " ++ show (e :: IOException)
        putStrLn "  Iniciando com inventário vazio."
        return Map.empty)
    else do
      putStrLn " Nenhum inventário anterior encontrado. Iniciando vazio."
      return Map.empty

carregarLogs :: IO [LogEntry]
carregarLogs = do
  existe <- doesFileExist arquivoAuditoria
  if existe
    then (do
      conteudo <- readFile arquivoAuditoria
      if null conteudo
        then do
          putStrLn "Arquivo de auditoria vazio."
          return []
        else do
          let linhas = lines conteudo
          let logs = map read linhas :: [LogEntry]
          putStrLn $ "Logs carregados: " ++ show (length logs) ++ " entradas"
          return logs)
    `catch`
      (\e -> do
        putStrLn $ "Erro ao ler logs: " ++ show (e :: IOException)
        putStrLn "  Iniciando com histórico vazio."
        return [])
    else do
      putStrLn "Nenhum log anterior encontrado. Iniciando vazio."
      return []

popularDadosIniciais :: IO Inventario
popularDadosIniciais = do
  putStrLn ""
  putStrLn "=== PRIMEIRA EXECUÇÃO ==="
  putStrLn "Populando inventário com dados iniciais..."
  putStrLn ""
  
  timestamp <- getCurrentTime
  
  let itensIniciais =
        [ ("MOUSE001", "Mouse Gamer", 25, "Periféricos")
        , ("TECL001", "Teclado Mecânico", 15, "Periféricos")
        , ("MONI001", "Monitor 24\"", 10, "Monitores")
        , ("WEBC001", "Webcam HD", 8, "Periféricos")
        , ("HEAD001", "Headset", 20, "Audio")
        , ("SSD001", "SSD 500GB", 12, "Armazenamento")
        , ("RAM001", "Memória RAM 8GB", 30, "Hardware")
        , ("MPAD001", "Mousepad", 50, "Acessórios")
        , ("HDMI001", "Cabo HDMI", 40, "Cabos")
        , ("HUB001", "Hub USB", 18, "Acessórios")
        ]
  
  let invInicial = foldr adicionarItem Map.empty itensIniciais
      adicionarItem (id, nm, qtd, cat) inv =
        case addItem timestamp id nm qtd cat inv of
          Right (novoInv, _) -> novoInv
          Left _ -> inv
  
  putStrLn $ " Criando arquivo " ++ arquivoInventario ++ "..."
  salvarInventario invInicial
  putStrLn $ " " ++ show (Map.size invInicial) ++ " itens salvos no inventário"
  putStrLn ""
  
  let logInicial = LogEntry
        { timestamp = timestamp
        , acao = Add
        , detalhes = "Sistema inicializado com " ++ show (length itensIniciais) ++ " itens"
        , status = Sucesso
        }
  
  putStrLn $ " Criando arquivo " ++ arquivoAuditoria ++ "..."
  adicionarLog logInicial
  putStrLn " Log de inicialização criado"
  putStrLn ""
  
  return invInicial

salvarInventario :: Inventario -> IO ()
salvarInventario inv = do
  writeFile arquivoInventario (show inv)
  return ()

adicionarLog :: LogEntry -> IO ()
adicionarLog log = do
  appendFile arquivoAuditoria (show log ++ "\n")
  return ()

loopPrincipal :: Inventario -> [LogEntry] -> IO ()
loopPrincipal inv logs = do
  putStrLn ""
  putStrLn "=== MENU PRINCIPAL ==="
  putStrLn "1. Adicionar Item"
  putStrLn "2. Remover Item"
  putStrLn "3. Atualizar Item"
  putStrLn "4. Listar Todos os Itens"
  putStrLn "5. Relatórios"
  putStrLn "6. Sair"
  putStr "Escolha uma opção: "
  hFlush stdout
  
  opcao <- getLine
  putStrLn ""
  
  case opcao of
    "1" -> menuAdicionar inv logs
    "2" -> menuRemover inv logs
    "3" -> menuAtualizar inv logs
    "4" -> menuListar inv logs
    "5" -> menuRelatorios inv logs
    "6" -> do
      putStrLn "Encerrando sistema..."
      putStrLn "Obrigado por usar o Sistema de Inventário!"
      return ()
    _ -> do
      putStrLn "Opção inválida! Tente novamente."
      loopPrincipal inv logs

menuAdicionar :: Inventario -> [LogEntry] -> IO ()
menuAdicionar inv logs = do
  putStrLn "=== ADICIONAR ITEM ==="
  
  putStr "ID do item: "
  hFlush stdout
  itemId <- getLine
  
  putStr "Nome do item: "
  hFlush stdout
  itemNome <- getLine
  
  putStr "Quantidade: "
  hFlush stdout
  qtdStr <- getLine
  
  putStr "Categoria: "
  hFlush stdout
  cat <- getLine
  
  timestamp <- getCurrentTime
  
  case reads qtdStr :: [(Int, String)] of
    [(qtd, "")] -> do
      case addItem timestamp itemId itemNome qtd cat inv of
        Right (novoInv, logEntry) -> do
          salvarInventario novoInv
          adicionarLog logEntry
          putStrLn ""
          putStrLn " Item adicionado com sucesso!"
          putStrLn $ "  " ++ detalhes logEntry
          putStrLn ""
          logsAtualizados <- carregarLogs
          loopPrincipal novoInv logsAtualizados
        Left erro -> do
          let logErro = LogEntry timestamp Add 
                ("Tentativa falha de adicionar: " ++ itemNome ++ " (ID: " ++ itemId ++ ")")
                (Falha erro)
          adicionarLog logErro
          putStrLn ""
          putStrLn $ "ERRO: " ++ erro
          putStrLn ""
          logsAtualizados <- carregarLogs
          loopPrincipal inv logsAtualizados
    _ -> do
      putStrLn ""
      putStrLn "Erro: Quantidade deve ser um número inteiro!"
      putStrLn ""
      loopPrincipal inv logs

menuRemover :: Inventario -> [LogEntry] -> IO ()
menuRemover inv logs = do
  putStrLn "=== REMOVER ITEM ==="
  
  putStr "ID do item: "
  hFlush stdout
  itemId <- getLine
  
  putStr "Quantidade a remover: "
  hFlush stdout
  qtdStr <- getLine
  
  timestamp <- getCurrentTime
  
  case reads qtdStr :: [(Int, String)] of
    [(qtd, "")] -> do
      case removeItem timestamp itemId qtd inv of
        Right (novoInv, logEntry) -> do
          salvarInventario novoInv
          adicionarLog logEntry
          putStrLn ""
          putStrLn " Item removido com sucesso!"
          putStrLn $ "  " ++ detalhes logEntry
          putStrLn ""
          logsAtualizados <- carregarLogs
          loopPrincipal novoInv logsAtualizados
        Left erro -> do
          let logErro = LogEntry timestamp Remove 
                ("Tentativa falha de remover do item ID: " ++ itemId)
                (Falha erro)
          adicionarLog logErro
          putStrLn ""
          putStrLn $ "ERRO: " ++ erro
          putStrLn ""
          logsAtualizados <- carregarLogs
          loopPrincipal inv logsAtualizados
    _ -> do
      putStrLn ""
      putStrLn "Erro: Quantidade deve ser um número inteiro!"
      putStrLn ""
      loopPrincipal inv logs

menuAtualizar :: Inventario -> [LogEntry] -> IO ()
menuAtualizar inv logs = do
  putStrLn "=== ATUALIZAR ITEM ==="
  putStrLn "(Deixe em branco para não alterar o campo)"
  putStrLn ""
  
  putStr "ID do item: "
  hFlush stdout
  itemId <- getLine
  
  putStr "Novo nome (Enter para manter): "
  hFlush stdout
  novoNome <- getLine
  
  putStr "Nova quantidade (Enter para manter): "
  hFlush stdout
  novaQtdStr <- getLine
  
  putStr "Nova categoria (Enter para manter): "
  hFlush stdout
  novaCat <- getLine
  
  timestamp <- getCurrentTime
  
  let novoNomeMaybe = if null novoNome then Nothing else Just novoNome
      novaCatMaybe = if null novaCat then Nothing else Just novaCat
      novaQtdMaybe = if null novaQtdStr
                     then Nothing
                     else case reads novaQtdStr :: [(Int, String)] of
                            [(qtd, "")] -> Just qtd
                            _ -> Nothing
  
  if not (null novaQtdStr) && novaQtdMaybe == Nothing
    then do
      putStrLn ""
      putStrLn "Erro: Quantidade deve ser um número inteiro!"
      putStrLn ""
      loopPrincipal inv logs
    else do
      case updateItem timestamp itemId novoNomeMaybe novaQtdMaybe novaCatMaybe inv of
        Right (novoInv, logEntry) -> do
          salvarInventario novoInv
          adicionarLog logEntry
          putStrLn ""
          putStrLn "Item atualizado com sucesso!"
          putStrLn $ "  " ++ detalhes logEntry
          putStrLn ""
          logsAtualizados <- carregarLogs
          loopPrincipal novoInv logsAtualizados
        Left erro -> do
          let logErro = LogEntry timestamp Update 
                ("Tentativa falha de atualizar item ID: " ++ itemId)
                (Falha erro)
          adicionarLog logErro
          putStrLn ""
          putStrLn $ "ERRO: " ++ erro
          putStrLn ""
          logsAtualizados <- carregarLogs
          loopPrincipal inv logsAtualizados

menuListar :: Inventario -> [LogEntry] -> IO ()
menuListar inv logs = do
  putStrLn "=== INVENTÁRIO COMPLETO ==="
  putStrLn ""
  
  if Map.null inv
    then putStrLn "Inventário vazio."
    else do
      putStrLn $ "Total de itens diferentes: " ++ show (Map.size inv)
      putStrLn ""
      mapM_ mostrarItem (Map.elems inv)
  
  putStrLn ""
  putStr "Pressione Enter para continuar..."
  hFlush stdout
  _ <- getLine
  loopPrincipal inv logs

mostrarItem :: Item -> IO ()
mostrarItem item = do
  putStrLn $ "- " ++ nome item
  putStrLn $ "  ID: " ++ itemID item
  putStrLn $ "  Quantidade: " ++ show (quantidade item)
  putStrLn $ "  Categoria: " ++ categoria item
  putStrLn ""

menuRelatorios :: Inventario -> [LogEntry] -> IO ()
menuRelatorios inv logs = do
  putStrLn "=== RELATÓRIOS ==="
  putStrLn "1. Histórico por Item"
  putStrLn "2. Logs de Erro"
  putStrLn "3. Item Mais Movimentado"
  putStrLn "4. Voltar"
  putStr "Escolha uma opção: "
  hFlush stdout
  
  opcao <- getLine
  putStrLn ""
  logsAtualizados <- carregarLogs
  
  case opcao of
    "1" -> do
      putStr "Digite o ID do item: "
      hFlush stdout
      itemId <- getLine
      putStrLn ""
      putStrLn $ relatorioHistoricoItem itemId logsAtualizados
      putStrLn ""
      putStr "Pressione Enter para continuar..."
      hFlush stdout
      _ <- getLine
      menuRelatorios inv logsAtualizados
      
    "2" -> do
      putStrLn $ relatorioErros logsAtualizados
      putStrLn ""
      putStr "Pressione Enter para continuar..."
      hFlush stdout
      _ <- getLine
      menuRelatorios inv logsAtualizados
      
    "3" -> do
      putStrLn $ relatorioItemMaisMovimentado logsAtualizados
      putStrLn ""
      putStr "Pressione Enter para continuar..."
      hFlush stdout
      _ <- getLine
      menuRelatorios inv logsAtualizados
      
    "4" -> loopPrincipal inv logsAtualizados
    
    _ -> do
      putStrLn "Opção inválida! Tente novamente."
      menuRelatorios inv logs

