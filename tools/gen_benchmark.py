#!/usr/bin/env python3

import math
import random

keywords = [
    "class",
    "else",
    "enum",
    "extends",
    "fn",
    "for",
    "if",
    "import",
    "in",
    "instance",
    "let",
    "match",
    "package",
    "return",
    "struct",
    "type",
    "where",
    "while",
    "with",
]


def biased_rand(n):
    return 1 + min(random.randrange(n), random.randrange(n))


def biased_rand4(n):
    return min(biased_rand(n), biased_rand(n))


def exp_rand(n):
    range = math.ceil(math.e ** n)
    result = 1 + random.randrange(range)
    return int(math.log(float(result)))


def pick_option(options):
    size = 0
    for (_, n) in options:
        size += n
    index = random.randrange(size)

    for (option, n) in options:
        if n > index:
            return option
        index -= n


def gen_letter():
    return chr(ord('a') + random.randrange(26))


def gen_cap_letter():
    return chr(ord('A') + random.randrange(26))


def gen_name():
    size = biased_rand(10)
    name = gen_letter()
    while len(name) < size:
        name += gen_letter()
    if name in keywords:
        return gen_name()
    return name


def gen_type_name(depth=0):
    size = biased_rand(7)
    name = gen_cap_letter()
    while len(name) < size:
        name += gen_letter()
    return name


def gen_package_decl():
    return ['package ' + gen_name()]


def gen_import():
    parts = [gen_name() for _ in range(biased_rand(4))]
    return ['import ' + '.'.join(parts)]


def gen_blank_line():
    return ['']


def gen_text():
    return ' '.join('lorem' for _ in range(random.randrange(10)))


def gen_line_comment():
    return ['// ' + gen_text()]


def gen_block_comment():
    n_lines = 1 + random.randrange(10)
    start = '/* ' + gen_text()
    lines = [start]
    for _ in  range(1, n_lines):
        lines.append('  ' + gen_text())
    lines[-1] += ' */'
    return lines


def gen_void_type(depth):
    return '()'


def gen_fn_type(depth):
    d = depth + 1
    args = ', '.join(gen_type(d) for _ in range(biased_rand(5)))
    return 'fn(' + args + ') ' + gen_type(d)


def gen_generic_type(depth):
    d = depth + 1
    args = ', '.join(gen_type(d) for _ in range(biased_rand(3)))
    return gen_type_name() + '<' + args + '>'


def gen_type(depth=0):
    if depth >= 4:
        return gen_type_name(depth)
    options = [
        (gen_void_type, 8),
        (gen_type_name, 20),
        (gen_fn_type, 2),
        (gen_generic_type, 2),
    ]
    return pick_option(options)(depth)


def gen_struct_decl():
    lines = ['type ' + gen_type_name() + ' struct:']
    n_fields = biased_rand(8)
    for _ in range(n_fields):
        field = '  ' + gen_name() + ' ' + gen_type()
        lines.append(field)
    return lines


def gen_type_alias():
    return ['type ' + gen_type_name() + ' ' + gen_type()]


def gen_enum_type():
    lines = ['type ' + gen_type_name() + ' enum:']
    n_variants = biased_rand(6)
    for  _ in range(n_variants):
        n_fields = biased_rand(4) - 1
        line = '  ' + gen_type_name()
        if n_fields == 0:
            lines.append(line)
        else:
            line += ':'
            lines.append(line)
            for _ in range(n_fields):
                field = '    ' + gen_name() + ' ' + gen_type()
                lines.append(field)
    return lines


def gen_class_type():
    lines = ['type ' + gen_type_name() + ' class:']
    n_functions = biased_rand(7)
    for _ in range(n_functions):
        args = ', '.join(gen_type() for _ in range(biased_rand(4)))
        lines.append('  fn ' + gen_name() + '(' + args + ') ' + gen_type_name())
    return lines


def gen_type_decl():
    options = [
        (gen_struct_decl, 3),
        (gen_type_alias, 2),
        (gen_enum_type, 1),
        (gen_class_type, 1),
    ]
    f = pick_option(options)
    return f()


def gen_int(depth=0):
    n_digits = biased_rand(6)
    digits = '0123456789'
    return ''.join(random.choice(digits) for _ in range(n_digits))


def gen_float(depth=0):
    return gen_int() + '.' + gen_int()


def gen_variable(depth=0):
    return gen_name()


def gen_string(depth=0):
    n_words = biased_rand4(10)
    content = ' '.join(gen_name() for _ in range(n_words))
    return '"{}"'.format(content)


def gen_struct_expr(depth=0):
    d = depth + 1
    n_fields = biased_rand4(4) - 1
    if n_fields == 0:
        return gen_type_name()
    fields = [(gen_name(), gen_expression(d)) for _ in range(n_fields)]
    fields_str = ', '.join('{}: {}'.format(name, expr) for (name, expr) in fields)
    return gen_type_name() + '{' + fields_str + '}'


def gen_value(depth=0):
    options = [
        (gen_int, 3),
        (gen_string, 2),
        (gen_float, 2),
        (gen_variable, 2),
    ]
    if depth < 4:
        options.append((gen_struct_expr, 1))
    return pick_option(options)(depth)


def gen_access(depth=0):
    return gen_term(depth + 1) + '.' + gen_name()


def gen_call(depth=0):
    n_args = biased_rand4(4)
    args = [gen_expression(depth + 1) for _ in range(n_args)]
    return gen_term(depth + 1) + '(' + ', '.join(args) + ')'


def gen_offset(depth=0):
    return gen_term(depth + 1) + '[' + gen_expression(depth + 1) + ']'


def gen_term(depth):
    if depth > 4:
        return gen_value(depth)
    options = [
        (gen_value, 10),
        (gen_access, 2),
        (gen_call, 1),
        (gen_offset, 2),
    ]
    return pick_option(options)(depth)


def gen_unary(depth):
    n_ops = biased_rand4(3) - 1
    unary_ops = ['-', '!', '~']
    ops = ''.join(random.choice(unary_ops) for _ in range(n_ops))
    return ops + gen_term(depth)


def gen_paren(depth):
    return '(' + gen_expression(depth + 1) + ')'


def gen_binary(depth):
    op = random.choice(['+', '-', '*', '/', '<', '<=', '>', '>=', '==', '!=', '%', '&&', '||'])
    l = gen_expression(depth + 1)
    r = gen_expression(depth + 1)
    return '{} {} {}'.format(l, op, r)


def gen_expression(depth=0):
    if depth > 4:
        return gen_unary(depth)
    options = [
        (gen_unary, 10),
        (gen_binary, 1),
    ]
    return pick_option(options)(depth)


def gen_blank_return(indent='', depth=0):
    return [indent + 'return']


def gen_expr_return(indent='', depth=0):
    return [indent + 'return ' + gen_expression(1)]


def gen_let(indent='', depth=0):
    return [indent + 'let ' + gen_name() + ' = ' + gen_expression(1)]


def gen_assign(indent='', depth=0):
    return [indent + gen_name() + ' = ' + gen_expression(1)]


def gen_expr_stmt(indent='', depth=0):
    return [indent + gen_expression(0)]


def gen_if(indent='', depth=0):
    d = depth + 1
    lines = [indent + 'if ' + gen_expression(1) + ':']
    n_body = biased_rand(5)
    for _ in range(n_body):
        lines.extend(gen_statement(indent + '  ', d))
    n_else = biased_rand4(4) - 1
    if n_else > 0:
        lines.append(indent + 'else:')
        for _ in range(n_else):
            lines.extend(gen_statement(indent + '  ', d))

    return lines


def gen_while(indent='', depth=0):
    lines = [indent + 'while ' + gen_expression(1) + ':']
    n_body = biased_rand4(5)
    for _ in range(n_body):
        lines.extend(gen_statement(indent + '  ', depth + 1))
    return lines


def gen_for(indent='', depth=0):
    lines = [indent + 'for ' + gen_name() + ' in ' + gen_expression(1) + ':']
    n_body = biased_rand4(5)
    for _ in range(n_body):
        lines.extend(gen_statement(indent + '  ', depth + 1))
    return lines


def gen_match(indent='', depth=0):
    lines = [indent + 'match ' + gen_expression(1) + ':']
    n_cases = biased_rand(5)
    indent2 = indent + '  '
    indent3 = indent2 + '  '
    for _ in range(n_cases):
        lines.append(indent2 + gen_type_name() + ':')
        n_statements = biased_rand(3)
        for _ in range(n_statements):
            lines.extend(gen_statement(indent3, depth + 1))
    return lines


def gen_statement(indent='', depth=0):
    options = [
        (gen_blank_return, 3),
        (gen_expr_return, 2),
        (gen_let, 4),
        (gen_assign, 10),
        (gen_expr_stmt, 10),
    ]

    if depth < 2:
        options += [
            (gen_if, 2),
            (gen_while, 2),
            (gen_for, 2),
            (gen_match, 1),
        ]

    return pick_option(options)(indent, depth)


def gen_function(indent=''):
    args = ', '.join(gen_name() + ' ' + gen_type() for _ in range(biased_rand(5)))
    lines = [indent + 'fn ' + gen_name() + '(' + args + ') ' + gen_type_name() + ':']

    indent += '  '
    n_statements = biased_rand(20)
    for _ in range(n_statements):
        lines.extend(gen_statement(indent))
    return lines


def gen_instance():
    lines = ['instance ' + gen_type_name() + ' ' + gen_type(2) + ':']
    indent = '  '
    n_func = biased_rand(5)
    for _ in range(n_func):
        lines.extend(gen_function(indent))
    return lines


def gen_decl():
    options = [
        (gen_package_decl, 1),
        (gen_import, 3),
        (gen_blank_line, 15),
        (gen_line_comment, 2),
        (gen_block_comment, 1),
        (gen_type_decl, 3),
        (gen_function, 7),
        (gen_instance, 4),
    ]
    f = pick_option(options)
    return f()


# Make this deterministic:
random.seed(987213498723)

if __name__ == '__main__':
    MIN_LINES = 1_000_000
    # MIN_LINES = 50000

    lines = []

    while len(lines) < MIN_LINES:
        lines.extend(gen_decl())

    with open('/tmp/benchmark.ik', 'w') as outf:
        for line in lines:
            outf.write(line)
            outf.write('\n')

    n = len(lines)
    print(f'wrote {n} lines')
