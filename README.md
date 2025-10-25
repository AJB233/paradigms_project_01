# CS4337 Project 1 – Prefix Calculator (Haskell)

**Author:** Arath Brosig  
**Course:** CS4337 – Programming Language Paradigms   

## Project Description
This project implements a **prefix-notation calculator** in **Haskell**.  
The program reads expressions in **prefix notation** (operators come first), evaluates them recursively, and maintains a **history** of results that can be reused in future expressions.

The calculator supports both:
- **Interactive mode** – prompts the user for input.
- **Batch mode** – reads commands from standard input and outputs only results and errors.

## Features
✅ **Operators Supported**
| Operator | Type | Description |
|-----------|------|-------------|
| `+` | Binary | Adds two expressions |
| `*` | Binary | Multiplies two expressions |
| `/` | Binary | Divides first expression by second (error on divide by zero) |
| `-` | Unary | Negates a single expression (no subtraction) |

✅ **History References**
- `$n` refers to the result of the **nth** previous computation.
- Example: `$1` means “use the first result ever computed”.
- History is immutable and numbered starting from 1.

✅ **Modes**
- **Interactive mode:** user types commands directly.  
- **Batch mode:** accepts `-b` or `--batch` flag and reads from standard input.

✅ **Error Handling**
- Detects invalid expressions
- Detects division by zero
- Detects invalid `$n` references
- Detects leftover tokens in an expression

✅ **Fully Functional REPL**
- Maintains history
- Prints results with incremental IDs
- Supports nested and mixed expressions

---

## 🖥️ Example Run

### **Interactive Mode**
$runhaskell src/Main.hs
Running in interactive mode.
Welcome to the Prefix Calculator!

2 3
1: 5.0

$1 4
2: 20.0

2 $1 + $2 1
3: 21.0
/ 10 $3
4: 0.47619047619047616
quit
Goodbye!

### **Batch Mode**
$ runhaskell src/Main.hs -b < input.txt

**input.txt**
2 3
$1 4
quit

**Output**
Running in batch mode.
1: 5.0
2: 20.0
Goodbye!

## Testing Scenarios

| Expression | Expected Result |
|-------------|----------------|
| `+ 2 3` | 5.0 |
| `* + 2 3 4` | 20.0 |
| `/ 9 3` | 3.0 |
| `- 10` | -10.0 |
| `* $1 2` | 10.0 |
| `+ * 2 $1 + $2 1` | 31.0 |

### Error Handling
| Input | Output |
|--------|---------|
| `/ 5 0` | `Error: Division by zero` |
| `$99` | `Error: Invalid history reference $99` |
| `+ 1 2 3` | `Error: Invalid Expression` |
| `+` | `Error: Invalid Expression` |

---

## Project Structure
project1/
│
├── src/
│ └── Main.hs # Main Haskell program
├── devlog.md # Development log
└── README.md # Project documentation