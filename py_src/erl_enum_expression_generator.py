#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import ast
import sys

from cv_depths import CV_CONSTANTS


class ErlEnumExpressionGenerator(ast.NodeVisitor):
    def __init__(self):
        self.expression = ''
        self.expression_erlang = ''
        self.skip_this = False

    def generic_visit(self, node):
        if type(node) is ast.Expression:
            self.visit(node.body)
        elif type(node) is ast.Constant:
            if node.value in ['I', 'Y', 'U', 'V', 'N']:
                self.expression = f'{ord(node.value)}'
                self.expression_erlang = f'{ord(node.value)}'
            else:
                self.expression = f'{node.value}'
                self.expression_erlang = f'{node.value}'
        elif type(node) is ast.UnaryOp:
            op = ErlEnumExpressionGenerator()
            op.visit(node.op)
            operand = ErlEnumExpressionGenerator()
            operand.visit(node.operand)
            self.expression = op.expression.format(operand.expression)
            self.expression_erlang = op.expression_erlang.format(operand.expression_erlang)
        elif type(node) is ast.UAdd:
            self.expression = '+{}'
            self.expression_erlang = '+{}'
        elif type(node) is ast.USub:
            self.expression = '-{}'
            self.expression_erlang = '-{}'
        elif type(node) is ast.BinOp:
            op = ErlEnumExpressionGenerator()
            op.visit(node.op)
            lhs = ErlEnumExpressionGenerator()
            lhs.visit(node.left)
            rhs = ErlEnumExpressionGenerator()
            rhs.visit(node.right)
            self.expression = op.expression.format(lhs.expression, rhs.expression)
            self.expression_erlang = op.expression_erlang.format(lhs.expression_erlang, rhs.expression_erlang)
        elif type(node) is ast.LShift:
            self.expression = 'bsl({}, {})'
            self.expression_erlang = '({} bsl {})'
        elif type(node) is ast.RShift:
            self.expression = 'bsr({}, {})'
            self.expression_erlang = '({} bsr {})'
        elif type(node) is ast.Name:
            if node.id[:3] == 'CV_':
                if node.id in CV_CONSTANTS:
                    value = str(CV_CONSTANTS[node.id])
                    self.expression = value
                    self.expression_erlang = value
                elif node.id == 'CV_MAT_CONT_FLAG':
                    self.skip_this = True
                elif node.id == 'CV_SUBMAT_FLAG':
                    self.skip_this = True
                else:
                    print(type(node), node.id, "not handled yet")
                    sys.exit(1)
            elif node.id == 'INT_MAX':
                # OpenCV 4.13 introduces enum sentinels like
                # `ENUM_LOG_LEVEL_FORCE_INT = INT_MAX` that force the
                # enum to int width. INT_MAX is a C++ macro the parser
                # doesn't know about — emit its numeric value directly.
                self.expression = '2147483647'
                self.expression_erlang = '2147483647'
            else:
                self.expression = f'cv_{node.id}()'
                self.expression_erlang = f'cv_{node.id}()'
        elif type(node) is ast.Mult:
            self.expression = '({} * {})'
            self.expression_erlang = '({} * {})'
        elif type(node) is ast.Add:
            self.expression = '({} + {})'
            self.expression_erlang = '({} + {})'
        elif type(node) is ast.Sub:
            self.expression = '({} - {})'
            self.expression_erlang = '({} - {})'
        elif type(node) is ast.BitAnd:
            self.expression = 'band({}, {})'
            self.expression_erlang = '({} band {})'
        elif type(node) is ast.Invert:
            self.expression = 'bnot({})'
            self.expression_erlang = 'bnot({})'
        elif type(node) is ast.BitOr:
            self.expression = 'bor({}, {})'
            self.expression_erlang = '({} bor {})'
        else:
            if sys.version_info.minor < 8:
                if type(node) is ast.Num:
                    self.expression = f'{node.n}'
                    self.expression_erlang = f'{node.n}'
                else:
                    print(type(node), "not implemented yet")
                    sys.exit(1)
            else:
                print(type(node), "not implemented yet")
                sys.exit(1)
