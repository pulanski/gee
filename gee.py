import re
import string
import sys
from typing import Dict, List

"""
Compiler front-end for the Gee Programming Language

Lexes, parses, and completes typechecking for gee programs following
the minimal grammar defined in `grammar.txt`.


Helpful variables used for storing the global state of compilation
throughout the phases of lexing, parsing, and context-sensitive
analysis (typecheck) during the compilation lifecycle
"""

# whether the compiler is running in debug mode
debug = False
# contains tokens created by the lexer during the lexical-analysis
# phase of the compiler (front-end)
tokens: List[str] = []
# contains state of the program over a walk of the AST
# during the analysis-phase of the compiler (front-end)
# (e.g. what variables are in scope and their values)
state = {}
# contains type information for the program during the
# analysis-phase of the compiler (front-end)
tm = {}

######################################
# Statement class and its subclasses #
######################################


class Statement(object):
    """
    Base class from which all statement-based nodes in the AST inherit from
    (introduced for readability).
    """

    def __str__(self) -> str:
        return ""


class StatementList(Statement):
    """
    Data Structure for ASTNode defining a statement list.
    A sequential list of statements
    (e.g. `if False: x = x + 5; if n < 12: a = b * n;`)
    Statements are defined to be either assignment, if, or  while statements.
    """

    def __init__(self):
        self.statements = []

    def __str__(self) -> str:
        statements_str = ""
        for i, statement in enumerate(self.statements):
            statements_str += str(statement)
            if i != len(self.statements) - 1:
                statements_str += "\n"
        return statements_str

    def append(self, statement):
        self.statements.append(statement)

    def meaning(self, state):
        for statement in self.statements:
            statement.meaning(state)
        return state

    def tipe(self, tm):
        for statement in self.statements:
            statement.tipe(tm)
        return tm


class IfStatement(Statement):
    """
    Data Structure for ASTNode defining an if statement.
    (e.g. `if False: x = x + 5`, `if n < 12: a = b * n`).
    An if statement in which if the expression evaluates to true,
    the if block is evaluated. Otherwise, if an else block exists,
    it is evaluated instead
    """

    def __init__(self, expr, if_block, else_block):
        self.expr = expr
        self.if_block = if_block
        self.else_block = else_block

    def __str__(self) -> str:
        if self.else_block is None:
            return "if " + str(self.expr) + "\n" + str(self.if_block) + "\nelse\nendif"
        else:
            return (
                "if "
                + str(self.expr)
                + "\n"
                + str(self.if_block)
                + "\nelse\n"
                + str(self.else_block)
                + "\nendif"
            )

    def meaning(self, state):
        if self.expr.value(state):
            self.if_block.meaning(state)
        else:
            if self.else_block:
                self.else_block.meaning(state)
        return state

    def tipe(self, tm):
        if self.expr.tipe(tm) != "boolean":
            # err_msg = "ERROR: Type Error: Expression found within while statement is not
            # boolean"
            err_msg = "Type Error: While expresion is not boolean!"
            error(err_msg)

        if self.else_block:
            self.else_block.tipe(tm)
        return tm


class WhileStatement(Statement):
    """
    Data Structure for ASTNode defining an while statement.
    (e.g. `while True: x = x + 5`, `while n > 1: f = f * n`)
    """

    def __init__(self, expr, while_block):
        self.expr = expr
        self.while_block = while_block

    def __str__(self) -> str:
        return "while " + str(self.expr) + "\n" + str(self.while_block) + "\nendwhile"

    def meaning(self, state):
        while self.expr.value(state):
            self.while_block.meaning(state)
        return state

    def tipe(self, tm):
        if self.expr.tipe(tm) != "boolean":
            # err_msg = "ERROR: Type Error: Expression found within while statement is not
            # boolean"
            err_msg = "Type Error: While expresion is not boolean!"
            error(err_msg)

        self.while_block.tipe(tm)
        return tm


class Assign(Statement):
    """
    Data Structure for ASTNode defining an assignment statement.
    (e.g. `x = 2`, `foo = "bar"`)
    """

    def __init__(self, ident, expr):
        self.ident = str(ident)
        self.expr = expr

    def __str__(self) -> str:
        return "= " + str(self.ident) + " " + str(self.expr)

    def meaning(self, state):
        state[self.ident] = self.expr.value(state)
        return state

    def tipe(self, tm):
        # If the variable name is in scope and there exists a different between
        # the type of the new expression and the current type of the variable
        # an error is thrown. Type casting is not allowed. Once a variable
        # is declared its type may not change

        if self.ident in tm:
            if tm[self.ident] != self.expr.tipe(tm):
                # err_msg = "ERROR: Type assertion failed. Type of "
                err_msg = (
                    "Type Error: " + tm[self.ident] + " = " + self.expr.tipe(tm) + "!"
                )
                error(err_msg)

        tm[self.ident] = self.expr.tipe(tm)

        print(self.ident, tm[self.ident])


#######################################
# Expression class and its subclasses #
#######################################


class Expression(object):
    """
    Base class from which all expression-based nodes in the AST inherit from
    (introduced for readability).
    """

    def __str__(self):
        return ""


class BinaryExpr(Expression):
    """
    Data Structure for ASTNode defining a binary expression.
    Binary expressions are the composition of two expressions
    separated by an operator
    """

    def __init__(self, op, left, right):
        self.op = op
        self.left = left
        self.right = right

    def __str__(self) -> str:
        return str(self.op) + " " + str(self.left) + " " + str(self.right)

    def value(self, state):
        left = self.left.value(state)
        right = self.right.value(state)

        # Arithmetic operators ("+" | "-" | "*" | "/")
        if self.op == "+":
            return left + right
        if self.op == "-":
            return left - right
        if self.op == "*":
            return left * right
        if self.op == "/":
            return left / right

        # Relational operators ("==" | "!=" | "<" | "<=" | ">" | ">=")
        if self.op == "==":
            return left == right
        if self.op == "!=":
            return left != right
        if self.op == "<":
            return left < right
        if self.op == "<=":
            return left <= right
        if self.op == ">":
            return left > right
        if self.op == ">=":
            return left >= right

        # Unreachable
        assert False

    def tipe(self, tm: Dict):
        left = self.left.tipe(tm)
        right = self.right.tipe(tm)

        if left == right:
            if left == "number" and self.op in ["+", "-", "*", "/"]:
                return "number"

            if left == "number" and self.op in [
                "==",
                "!=",
                "<",
                "<=",
                ">",
                ">=",
            ]:
                return "boolean"

            if left == "boolean" and self.op in [
                "and",
                "or",
            ]:
                return "boolean"

        # err_msg = "ERROR: Type assertion failed. Type of "
        err_msg = "Type Error: " + left + " = " + right + "!"
        error(err_msg)


#####################################
# Terminal class and its subclasses #
#####################################


class Terminal(object):
    """
    Base class from which all terminal nodes in the AST inherit from.
    (introduced for readability).
    """

    def __str__(self) -> str:
        return ""


class VarRef(Terminal):
    """
    Data Structure for ASTNode defining a variable reference.
    Identifiers for variable names introduced into the scope of execution
    (e.g. `x`, `foo`, `bar`, etc.)
    """

    def __init__(self, ident):
        self.ident = ident
        # self.tipe

    def __str__(self) -> str:
        return str(self.ident)

    def value(self, state):
        return state[self.ident]

    def tipe(self, tm):
        if self.ident in tm.keys():
            return tm[self.ident]

        err_msg = "Type Error: " + self.ident + " is referenced before being defined!"
        error(err_msg)


class String(Terminal):
    """
    Data Structure for ASTNode defining a binary expression.
    String literals (e.g. `'hello'`, `"hello world"`, etc.)
    """

    def __init__(self, val):
        self.val = val

    def __str__(self) -> str:
        return str(self.val)

    def value(self, state):
        return str(self.val)

    def tipe(self, tm):
        return "string"


class Number(Terminal):
    """
    Data Structure for ASTNode defining a numeric literal.
    Numeric literals (e.g. `8`, `32.39`, etc.)
    (no distinction is made between integers and floating point numbers
    from the perspective of the lexer and as such the parser as well)
    """

    def __init__(self, val):
        self.val = int(val)

    def __str__(self) -> str:
        return str(self.val)

    def value(self, state):
        return int(self.val)

    def tipe(self, tm):
        return "number"


class Boolean(Terminal):
    """
    Data Structure for ASTNode defining a boolean literal.
    Boolean literals (e.g. `true`, `True`, `false`, `False`, etc.)
    """

    def __init__(self, val):
        self.val = val

    def __str__(self) -> str:
        return str(self.val)

    def value(self, state):
        return self.val.lower() in ["true"]

    def tipe(self, tm):
        return "boolean"


###################################
# Root (uppermost) parse routines #
###################################


def parse(text):
    """
    Recursive descent parser for a Gee program. Takes the tokens emitted from
    the lexer and returns an AST of the program derived from top-down parsing.
    """

    global tokens
    tokens = Lexer(text)
    return parseStmtList()


def parseStmtList():
    """
    Root production for the grammar of a Gee program, consisting of a list of statments.
    Parses a statement list according to the following grammar rule:
    gee = { Statement }
    """

    tok = tokens.peek()
    statement_list = StatementList()
    while tok is not None and tok != "~":
        stmt = parseStmt(tokens)
        statement_list.append(stmt)
        tok = tokens.peek()

    return statement_list


def parseStmt(tokens):
    """
    Parses statements according to the following grammar rule:
    statement = ifStatement |  whileStatement  |  assign
    """

    tok = tokens.peek()
    if debug:
        print("Statement: ", tok)

    if tok == "if":
        return ifStatement()
    elif tok == "while":
        return whileStatement()
    elif re.match(Lexer.identifier, tok):
        return assign()

    error(
        "Invalid statement. Must be of the form ifStatement or whileStatement"
        + " or assign"
    )


#################################
# Parse routines for statements #
#################################


def assign():
    """
    Parses assignment statments according to the following grammar:
    assign = ident '=' expression eoln
    """

    tok = tokens.peek()
    if debug:
        print("Assign: ", tok)

    # parse identifier
    if re.match(Lexer.identifier, tok):
        identifier = ident()

    tok = tokens.peek()
    if tok == "=":
        match(tok)  # skip over the equals symbol

    expr = expression()

    tokens.next()  # skip the end of statement token, `;`
    assign = Assign(identifier, expr)

    assign.tipe(tm)
    assign.meaning(state)

    return assign


def whileStatement():
    """
    Parses while statments according to the following grammar:
    whileStatement = 'while' expression block
    """

    tok = tokens.peek()
    if debug:
        print("whileStatement: ", tok)

    if tok == "while":
        match(tok)  # or tokens.next()
        expr = expression()  # get the expression
        blk = block()  # get the following block
        return WhileStatement(expr, blk)

    error("Invalid while statement")


def ifStatement():
    """
    Parses if statments according to the following grammar:
    ifStatement = 'if' expression block [ 'else' block ]
    """

    tok = tokens.peek()
    if debug:
        print("ifStatement: ", tok)

    if tok == "if":
        match(tok)  # or tokens.next()
        expr = expression()  # get the expression
        if_blk = block()  # get the following block
        tok = tokens.peek()
        else_blk = None
        if tok == "else":
            match(tok)
            else_blk = block()
        return IfStatement(expr, if_blk, else_blk)

    error("Invalid if statement")


def block():
    """
    Parses block statements according to the following grammar:
    block = ':' eoln indent stmtList undent
    """

    tok = tokens.peek()
    if debug:
        print("block: ", tok)

    if tok == ":":
        match(tok)
        tok = tokens.peek()
        if tok == ";":
            match(tok)
            tok = tokens.peek()
            if tok == "@":
                match(tok)
                stmts = parseStmtList()
                tok = tokens.peek()
                if tok == "~":
                    match(tok)
                    return stmts

    error("Invalid block")


##################################
# Parse routines for expressions #
##################################


def expression():
    """
    Parses expressions according to the following grammar:
    expression = andExpr { 'or' relationalExpr }
    """

    tok = tokens.peek()
    if debug:
        print("expression: ", tok)

    left = andExpr()

    tok = tokens.peek()
    while tok == "or":
        tokens.next()
        right = andExpr()
        left = BinaryExpr(tok, left, right)
        tok = tokens.peek()

    # skip the next token -
    # needed if expressions themselves are valid in the grammar
    # tokens.next()
    return left


def andExpr():
    """
    Parses and expressions according to the following grammar:
    andExpr = relationalExpr { 'and' relationalExpr }
    """

    tok = tokens.peek()
    if debug:
        print("andExpr: ", tok)

    left = relationalExpr()

    tok = tokens.peek()
    while tok == "and":
        tokens.next()
        right = relationalExpr()
        left = BinaryExpr(tok, left, right)
        tok = tokens.peek()
    return left


def relationalExpr():
    """
    Parses relational expressions according to the following grammar:
    relationalExpr = addExpr [ relation addExpr ]
    """

    tok = tokens.peek()
    if debug:
        print("relationalExpr: ", tok)

    left_expr = addExpr()

    tok = tokens.peek()
    if (
        tok == "=="
        or tok == "!="
        or tok == "<"
        or tok == "<="
        or tok == ">"
        or tok == ">="
    ):
        tokens.next()
        right_expr = addExpr()
        relation = BinaryExpr(tok, left_expr, right_expr)
        return relation
    else:
        return left_expr


def addExpr():
    """
    Parses add expressions according to the following grammar:
    addExpr    = term { ('+' | '-') term }
    """

    tok = tokens.peek()
    if debug:
        print("addExpr: ", tok)
    left = term()
    tok = tokens.peek()
    while tok == "+" or tok == "-":
        tokens.next()
        right = term()
        left = BinaryExpr(tok, left, right)
        tok = tokens.peek()
    return left


def term():
    """
    Parses terms according to the following grammar:
    term    = factor { ('*' | '/') factor }
    """

    tok = tokens.peek()
    if debug:
        print("Term: ", tok)
    left = factor()
    tok = tokens.peek()
    while tok == "*" or tok == "/":
        tokens.next()
        right = factor()
        left = BinaryExpr(tok, left, right)
        tok = tokens.peek()
    return left


def factor():
    """
    Parses factors according to the following grammar:
    factor     = number | string | ident |  '(' expression ')'
    """

    tok = tokens.peek()
    if debug:
        print("Factor: ", tok)

    # parse number
    if re.match(Lexer.number, tok):
        expr = Number(tok)
        tokens.next()
        return expr

    # parse boolean
    if re.match(Lexer.boolean, tok):
        expr = Boolean(tok)
        tokens.next()
        return expr

    # parse identifier
    if re.match(Lexer.identifier, tok):
        return ident()

    # parse string
    if re.match(Lexer.string, tok):
        expr = String(tok)
        tokens.next()
        return expr

    # parse expression surrounded by parentheses
    if tok == "(":
        match(tok)  # or tokens.next()
        expr = expression()
        tok = tokens.peek()
        match(tok)
        return expr

    error(error_message("parsing", "string, variable, number, or boolean", tok))


def ident():
    """
    Helper function for identifiers
    used both in assign and factor
    """

    tok = tokens.peek()
    if debug:
        print("Ident: ", tok)

    expr = VarRef(tok)
    tokens.next()
    return expr


def match(matchtok):
    """
    Helper function for matching tokens during parsing routines. Will
    print expected token and token found when error occurs.
    """

    tok = tokens.peek()
    if tok != matchtok:
        error_msg = (
            "ERROR: Invalid syntax. Expected token `"
            + matchtok
            + "` but found token `"
            + tok
            + "`."
        )

        error(error_msg)
    tokens.next()
    return tok


# Lexer, a class that represents lists of tokens from a Gee
# statement. This class provides the following to its clients:
#
#   o A constructor that takes a string representing a statement
#       as its only parameter, and that initializes a sequence with
#       the tokens from that string.
#
#   o peek, a parameterless message that returns the next token
#       from a token sequence. This returns the token as a string.
#       If there are no more tokens in the sequence, this message
#       returns None.
#
#   o removeToken, a parameterless message that removes the next
#       token from a token sequence.
#
#   o __str__, a parameterless message that returns a string representation
#       of a token sequence, so that token sequences can print nicely


class Lexer:
    # The constructor with some regular expressions that define Gee's lexical
    # rules. The constructor uses these expressions to split the input
    # expression into a list of substrings that match Gee tokens, and saves
    # that list to be doled out in response to future "peek" messages. The
    # position in the list at which to dole next is also saved for "nextToken"
    # to use.

    special = r"\(|\)|\[|\]|,|:|;|@|~|;|\$"
    relational = "<=?|>=?|==?|!="
    arithmetic = "\+|\-|\*|/"
    # char = r"'."
    string = r"'[^']*'" + "|" + r'"[^"]*"'
    number = r"\-?\d+(?:\.\d+)?"
    boolean = "true|True|false|False|missing|Missing"
    literal = string + "|" + number + "|" + boolean
    # idStart = r"a-zA-Z"
    # idChar = idStart + r"0-9"
    # identifier = "[" + idStart + "][" + idChar + "]*"
    identifier = "[a-zA-Z]\w*"
    lexRules = (
        literal + "|" + special + "|" + relational + "|" + arithmetic + "|" + identifier
    )

    def __init__(self, text):
        self.tokens = re.findall(Lexer.lexRules, text)
        self.position = 0
        self.indent = [0]

    # The peek method. This just returns the token at the current position in
    # the list, or None if the current position is past the end of the list.

    def peek(self):
        if self.position < len(self.tokens):
            return self.tokens[self.position]
        else:
            return None

    # The removeToken method. All this has to do is increment the token
    # sequence's position counter.

    def next(self):
        self.position = self.position + 1
        return self.peek()

    # An "__str__" method, so that token sequences print in a useful form.

    def __str__(self):
        return "<Lexer at " + str(self.position) + " in " + str(self.tokens)
        +">"


def chkIndent(line):
    ct = 0
    for ch in line:
        if ch != " ":
            return ct
        ct += 1
    return ct


def delComment(line):
    pos = line.find("#")
    if pos > -1:
        line = line[0:pos]
        line = line.rstrip()
    return line


def mklines(filename):
    inn = open(filename, "r")
    lines = []
    pos = [0]
    ct = 0
    for line in inn:
        ct += 1
        line = line.rstrip() + ";"
        line = delComment(line)
        if len(line) == 0 or line == ";":
            continue
        indent = chkIndent(line)
        line = line.lstrip()
        if indent > pos[-1]:
            pos.append(indent)
            line = "@" + line
        elif indent < pos[-1]:
            while indent < pos[-1]:
                del pos[-1]
                line = "~" + line
        print(ct, "\t", line)
        lines.append(line)
    # print len(pos)
    undent = ""
    for _ in pos[1:]:
        undent += "~"
    lines.append(undent)
    # print undent
    return lines


def error(msg):
    """
    Helper function for exiting and displaying the provided error message
    """
    # print msg
    sys.exit(msg)


def error_message(error_type: str, expected: str, found: str):
    """
    Helper function for building errors and displaying them to
    the user during the different phases of compilation.
    """

    err_msg = "ERROR: "
    if error_type == "parsing":
        err_msg += (
            "Invalid syntax. "
            + "Expected "
            + expected
            + ", but found token `"
            + found
            + "`."
        )
    elif error_type == "typecheck":
        pass
        err_msg += ""

    return err_msg


# def typecheck(program_ast: StatementList):
#     """
#     Typecheck the program reporting any type errors encountered
#     over the course of walking the AST checking the types on
#     needed nodes.
#     """
#
#     print()
# program_ast.tipe(tm)


def print_state(state: Dict):
    """
    Print the evaluated state of the program (useful for debugging).
    """

    state_display = "\n{"
    for key, val in state.items():
        state_display += "<" + str(key) + ", " + str(val) + ">, "

    state_display = state_display[:-2]
    state_display += "}"

    print(state_display)


def main():
    """
    Main program for testing.
    """

    global debug
    ct = 0
    for opt in sys.argv[1:]:
        if opt[0] != "-":
            break
        ct = ct + 1
        if opt == "-d":
            debug = True
    if len(sys.argv) < 2 + ct:
        print("Usage:  %s filename" % sys.argv[0])
        return
    print("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")
    program_ast = parse("".join(mklines(sys.argv[1 + ct])))
    program_ast.meaning(state)
    # print(program_ast)
    # print_state(state)
    # typecheck(program_ast)
    return


main()
