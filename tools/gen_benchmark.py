#!/usr/bin/env python3

import random





def biased_rand(n):
    return 1 + min(random.randrange(n), random.randrange(n))


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


def gen_statement(indent):
    return [indent + 'return'] # TODO


def gen_function(indent=''):
    args = ', '.join(gen_name() + ' ' + gen_type() for _ in range(biased_rand(5)))
    lines = [indent + 'fn ' + gen_name() + '(' + args + ') ' + gen_type_name() + ':']

    indent += '  '
    n_statements = biased_rand(20)
    for _ in range(n_statements):
        lines.extend(gen_statement(indent))
    return lines


def gen_instance():
    return [] # TODO


def gen_decl():
    options = [
        (gen_package_decl, 1),
        (gen_import, 3),
        (gen_blank_line, 3),
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

# MIN_LINES = 1_000_000
MIN_LINES = 50

lines = []

while len(lines) < MIN_LINES:
    lines.extend(gen_decl())

with open('/tmp/benchmark.ik', 'w') as outf:
    for line in lines:
        outf.write(line)
        outf.write('\n')

n = len(lines)
print(f'wrote {n} lines')
