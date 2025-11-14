# Sistema de Inventário em Haskell

## Informações Acadêmicas

**Instituição:** [PUCPR]  
**Disciplina:** Programação Lógica e Funcional  
**Professor:** [FRANK COELHO DE ALCANTARA]
**Período:** 4º Período  

### Integrantes do Grupo (Ordem Alfabética)

1. **André Esteves Arantes** - GitHub: [@DJoverdant](https://github.com/DJoverdant)
   - Responsável por: Types.hs

2. **Fernando Aschwanden** - GitHub: [@Fer604](https://github.com/Fer604)
   - Responsável por: Logic.hs e Reports.hs 

3. **Gabriel Zem Muraro** - GitHub: [@Gabriel-Zem-Muraro](https://github.com/Gabriel-Zem-Muraro)
   - Responsável por: Main.hs e README.md

---

## 🔗 Links Importantes

- **Executar o Projeto:** [Link do OnlineGDB](https://onlinegdb.com/EG2Yvb99S) 
- **Repositório GitHub:** [Link do Repositório](https://github.com/Gabriel-Zem-Muraro/RA2-9) 

---

## 📋 Descrição do Projeto

Este é um sistema de gerenciamento de inventário desenvolvido em Haskell que demonstra conceitos fundamentais de programação funcional, incluindo:

- **Funções Puras**: Toda a lógica de negócio é implementada sem efeitos colaterais
- **Separação de Responsabilidades**: Divisão clara entre lógica pura e operações de I/O
- **Persistência de Dados**: Estado do inventário salvo em arquivo
- **Auditoria Completa**: Registro de todas as operações em log append-only
- **Tratamento de Erros**: Uso de `Either` para validações e `catch` para exceções de I/O

### Funcionalidades

Adicionar novos itens ao inventário  
Remover quantidades de itens existentes  
Atualizar informações de itens (nome, quantidade, categoria)  
Listar todos os itens cadastrados  
Gerar relatórios de auditoria:
Histórico de operações por item
Logs de erros
Item mais movimentado  
Persistência automática em arquivos  
População automática com 10 itens na primeira execução

---

## 🏗️ Arquitetura do Sistema

O projeto está dividido em 4 módulos principais:

### 1. **Types.hs** - Tipos de Dados

Define todas as estruturas de dados do sistema:

- `Item`: Representa um produto (ID, nome, quantidade, categoria)
- `Inventario`: Mapa de itens indexados por ID
- `AcaoLog`: Enumeração de tipos de ação (Add, Remove, Update, QueryFail)
- `StatusLog`: Resultado da operação (Sucesso, Falha)
- `LogEntry`: Entrada de log de auditoria

Todos os tipos derivam `Show` e `Read` para serialização.

### 2. **Logic.hs** - Lógica de Negócio Pura

Contém funções puras que implementam as regras de negócio:

- `addItem`: Adiciona novo item com validações (ID único, campos não vazios)
- `removeItem`: Remove quantidade com verificação de estoque
- `updateItem`: Atualiza campos do item (nome, quantidade, categoria)
- `updateQty`: Alias simplificado para atualizar apenas a quantidade (conforme especificação)

Todas as funções retornam `Either String ResultadoOperacao` para tratamento de erros.

### 3. **Reports.hs** - Análise e Relatórios

Funções puras de análise sobre logs:

- `historicoPorItem`: Filtra operações de um item específico
- `logsDeErro`: Retorna apenas operações com falha
- `itemMaisMovimentado`: Identifica o item com mais operações

### 4. **Main.hs** - I/O e Persistência

Gerencia toda a interação com o usuário e sistema de arquivos:

- Loop interativo com menu
- Leitura/escrita de `Inventario.dat`
- Append de `Auditoria.log`
- Tratamento de exceções com `catch`
- População automática na primeira execução


## 🎮 Guia de Uso

### Menu Principal

Ao executar o programa, você verá:

```
=== SISTEMA DE INVENTÁRIO ===
Inicializando sistema...

=== MENU PRINCIPAL ===
1. Adicionar Item
2. Remover Item
3. Atualizar Item
4. Listar Todos os Itens
5. Relatórios
6. Sair
Escolha uma opção:
```

### Exemplos de Comandos

#### 1. Adicionar Item

```
Escolha uma opção: 1

=== ADICIONAR ITEM ===
ID do item: MOUSE002
Nome do item: Mouse Wireless
Quantidade: 30
Categoria: Periféricos

✓ Item adicionado com sucesso!
```

#### 2. Remover Item

```
Escolha uma opção: 2

=== REMOVER ITEM ===
ID do item: MOUSE001
Quantidade a remover: 5

✓ Item removido com sucesso!
```

#### 3. Atualizar Item

```
Escolha uma opção: 3

=== ATUALIZAR ITEM ===
(Deixe em branco para não alterar o campo)

ID do item: TECL001
Novo nome (Enter para manter): Teclado RGB
Nova quantidade (Enter para manter): 20
Nova categoria (Enter para manter): 

✓ Item atualizado com sucesso!
```

#### 4. Listar Todos os Itens

```
Escolha uma opção: 4

=== INVENTÁRIO COMPLETO ===

Total de itens diferentes: 10

• Mouse Gamer
  ID: MOUSE001
  Quantidade: 25
  Categoria: Periféricos

• Teclado Mecânico
  ID: TECL001
  Quantidade: 15
  Categoria: Periféricos
...
```

#### 5. Relatórios

```
Escolha uma opção: 5

=== RELATÓRIOS ===
1. Histórico por Item
2. Logs de Erro
3. Item Mais Movimentado
4. Voltar
```

---

## 🧪 Cenários de Teste Executados

### Cenário 1: Persistência de Estado (Sucesso)

**Objetivo:** Verificar que o inventário é salvo e carregado corretamente entre execuções.

**Passos:**
1. Executar o programa pela primeira vez (sem arquivos de dados)
2. Sistema popula automaticamente com 10 itens
3. Adicionar 3 novos itens:
   - ID: "NOTE001", Nome: "Notebook Dell", Qtd: 5, Cat: "Computadores"
   - ID: "IMPR001", Nome: "Impressora HP", Qtd: 3, Cat: "Impressoras"
   - ID: "ROUT001", Nome: "Roteador TP-Link", Qtd: 7, Cat: "Redes"
4. Escolher opção "6. Sair" para encerrar
5. Verificar que os arquivos `Inventario.dat` e `Auditoria.log` foram criados no diretório
6. Executar o programa novamente
7. Escolher opção "4. Listar Todos os Itens"

**Resultado Esperado:**
- ✅ Arquivos criados com sucesso
- ✅ Ao reiniciar, o sistema exibe: "✓ Inventário carregado: 13 itens"
- ✅ A listagem mostra os 10 itens iniciais + os 3 adicionados
- ✅ Todas as quantidades estão corretas

**Status:** ✅ **PASSOU**

---

### Cenário 2: Erro de Lógica (Estoque Insuficiente)

**Objetivo:** Verificar que o sistema valida corretamente tentativas de remoção com estoque insuficiente.

**Passos:**
1. No menu principal, escolher opção "2. Remover Item"
2. Digitar ID: "WEBC001" (Webcam HD, que tem 8 unidades)
3. Digitar quantidade a remover: 15
4. Observar a mensagem de erro
5. Escolher opção "4. Listar Todos os Itens" e verificar a quantidade de WEBC001
6. Escolher opção "5. Relatórios" → "2. Logs de Erro"

**Resultado Esperado:**
- ✅ Sistema exibe: "✗ Erro: Estoque insuficiente. Disponível: 8, Solicitado: 15"
- ✅ A listagem mostra que WEBC001 ainda tem 8 unidades (não foi alterado)
- ✅ O arquivo `Inventario.dat` não foi modificado
- ✅ O arquivo `Auditoria.log` contém uma entrada com:
  - acao: Remove
  - status: Falha "Erro: Estoque insuficiente..."
  - detalhes: "Tentativa falha de remover do item ID: WEBC001"

**Status:** ✅ **PASSOU**

---

### Cenário 3: Geração de Relatório de Erros

**Objetivo:** Verificar que o sistema gera relatórios corretos sobre operações com falha.

**Passos:**
1. Executar várias operações com erros intencionais:
   - Tentar adicionar item com ID duplicado (ex: "MOUSE001")
   - Tentar remover de item inexistente (ex: "ITEM999")
   - Tentar atualizar item inexistente (ex: "TESTE000")
   - Tentar adicionar item com quantidade negativa
2. Escolher opção "5. Relatórios" → "2. Logs de Erro"
3. Analisar o relatório gerado

**Resultado Esperado:**
- ✅ O relatório "RELATÓRIO DE ERROS" exibe todas as 4+ falhas registradas
- ✅ Cada entrada mostra:
  - Número sequencial
  - Tipo de ação [Add], [Remove] ou [Update]
  - Detalhes da operação
  - Motivo da falha
- ✅ O total de erros é contabilizado corretamente
- ✅ Todas as falhas do Cenário 2 também aparecem

**Exemplo de Saída:**
```
=== RELATÓRIO DE ERROS ===

1. [Remove] Tentativa falha de remover do item ID: WEBC001
   Motivo: Erro: Estoque insuficiente. Disponível: 8, Solicitado: 15

2. [Add] Tentativa falha de adicionar: Mouse Gamer (ID: MOUSE001)
   Motivo: Erro: Item com ID 'MOUSE001' já existe no inventário

3. [Remove] Tentativa falha de remover do item ID: ITEM999
   Motivo: Erro: Item com ID 'ITEM999' não encontrado

Total de erros: 3
```

**Status:** ✅ **PASSOU**

---

## 📊 Dados de Teste Iniciais

O sistema é automaticamente populado com 10 itens na primeira execução:

| ID       | Nome              | Quantidade | Categoria      |
|----------|-------------------|------------|----------------|
| MOUSE001 | Mouse Gamer       | 25         | Periféricos    |
| TECL001  | Teclado Mecânico  | 15         | Periféricos    |
| MONI001  | Monitor 24"       | 10         | Monitores      |
| WEBC001  | Webcam HD         | 8          | Periféricos    |
| HEAD001  | Headset           | 20         | Audio          |
| SSD001   | SSD 500GB         | 12         | Armazenamento  |
| RAM001   | Memória RAM 8GB   | 30         | Hardware       |
| MPAD001  | Mousepad          | 50         | Acessórios     |
| HDMI001  | Cabo HDMI         | 40         | Cabos          |
| HUB001   | Hub USB           | 18         | Acessórios     |

---

## 📁 Arquivos de Persistência

O sistema cria dois arquivos automaticamente:

### `Inventario.dat`

Contém o estado atual do inventário serializado. É sobrescrito a cada operação bem-sucedida.

**Formato:** Representação `Show` de `Map String Item`

### `Auditoria.log`

Contém o histórico completo de todas as operações (sucessos e falhas) em modo append-only.

**Formato:** Representação `Show` de `[LogEntry]`

---

## 🎯 Conceitos de Programação Funcional Aplicados

### Funções Puras
Todas as funções em `Logic.hs` e `Reports.hs` são puras:
- Sempre retornam o mesmo resultado para a mesma entrada
- Não produzem efeitos colaterais
- Não dependem de estado externo

### Separação de Responsabilidades
- **Lógica de negócio**: Funções puras em `Logic.hs`
- **I/O e efeitos**: Confinado em `Main.hs`
- **Tipos**: Definidos em `Types.hs`
- **Análise**: Funções puras em `Reports.hs`

### Imutabilidade
Nenhuma estrutura de dados é modificada - sempre criamos novas versões:
```haskell
novoInv = Map.insert itemId novoItem inv  -- inv não é modificado
```

### Tratamento de Erros com Either
```haskell
addItem :: ... -> Either String ResultadoOperacao
-- Left "mensagem de erro"  -- Em caso de falha
-- Right (inventario, log)  -- Em caso de sucesso
```

### Pattern Matching
Usado extensivamente para verificar resultados:
```haskell
case addItem timestamp id nome qtd cat inv of
  Right (novoInv, logEntry) -> -- Sucesso
  Left erro -> -- Falha
```

---

## 🛠️ Tecnologias Utilizadas

- **Linguagem:** Haskell (GHC 8.10+)
- **Estruturas de Dados:** Data.Map (para o inventário)
- **Tempo:** Data.Time (para timestamps)
- **I/O:** System.IO, System.Directory
- **Exceções:** Control.Exception

---

## 📝 Divisão de Tarefas

### Aluno 1 - Arquiteto de Dados
- ✅ Definição de todos os tipos (`Item`, `Inventario`, `AcaoLog`, `StatusLog`, `LogEntry`)
- ✅ Garantia de derivação correta de `Show` e `Read`
- ✅ Documentação dos tipos

### Aluno 2 - Lógica de Negócio
- ✅ Implementação de `addItem` com validações
- ✅ Implementação de `removeItem` com validações
- ✅ Implementação de `updateItem` com validações
- ✅ Implementação de `updateQty` (alias conforme especificação)
- ✅ Uso correto de `Either` para tratamento de erros
- ✅ Funções 100% puras (sem I/O)

### Aluno 3 - I/O, Relatórios e Documentação
- ✅ Implementação do `main` e loop interativo
- ✅ Persistência (`writeFile`, `appendFile`)
- ✅ Tratamento de exceções com `catch`
- ✅ Funções de análise (`historicoPorItem`, `logsDeErro`, `itemMaisMovimentado`)
- ✅ Menu interativo completo
- ✅ População automática dos 10 itens
- ✅ Este README.md
- ✅ Execução e documentação dos cenários de teste

---

## 🐛 Tratamento de Erros

O sistema implementa dois níveis de tratamento de erros:

### 1. Erros de Lógica de Negócio (Either)
Validações implementadas:
- ✅ ID duplicado ao adicionar
- ✅ Item não encontrado ao remover/atualizar
- ✅ Estoque insuficiente ao remover
- ✅ Campos vazios
- ✅ Quantidade negativa

### 2. Erros de I/O (catch)
Exceções tratadas:
- ✅ Arquivo não existe (primeira execução)
- ✅ Erro de leitura de arquivo
- ✅ Arquivo corrompido (read falha)

---

## 📖 Como Entender o Código (Para Iniciantes)

### O que é uma Entity?
Uma **entity** (entidade) representa algo do mundo real. No nosso sistema, `Item` é uma entity:
```haskell
data Item = Item
  { itemID :: String
  , nome :: String
  , quantidade :: Int
  , categoria :: String
  }
```

### O que é um Repository?
Um **repository** é onde guardamos nossas entities. O `Inventario` é nosso repository:
```haskell
type Inventario = Map String Item
-- Mapa que associa ID do item → Item completo
```

### Funções Puras vs Impuras

**Função Pura** (sempre retorna o mesmo resultado):
```haskell
soma :: Int -> Int -> Int
soma x y = x + y
-- soma 2 3 sempre será 5
```

**Função Impura** (pode ter resultados diferentes):
```haskell
getCurrentTime :: IO UTCTime
-- Retorna tempo diferente a cada chamada!
```

---



---

## 📞 Suporte

Em caso de dúvidas sobre o código, consulte os comentários nos arquivos fonte ou entre em contato com os membros do grupo através do GitHub.

---

## 📄 Licença

Este projeto foi desenvolvido para fins acadêmicos como parte da disciplina de Programação Lógica e Funcional.

---

**Desenvolvido com ❤️ em Haskell**

