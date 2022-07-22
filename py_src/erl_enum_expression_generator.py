#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import ast
import sys

class ErlEnumExpressionGenerator(ast.NodeVisitor):
    def __init__(self):
        self.expression = ''
        self.expression_erlang = ''
        self.skip_this = False

    def generic_visit(self, node):
        if type(node) is ast.Expression:
            self.visit(node.body)
        elif type(node) is ast.Constant:
            self.expression = f'{node.value}'
            self.expression_erlang = f'{node.value}'
        elif type(node) is ast.UnaryOp:
            op = ErlEnumExpressionGenerator()
            op.visit(node.op)
            operand = ErlEnumExpressionGenerator()
            operand.visit(node.operand)
            self.expression = op.expression.format(operand.expression)
            self.expression_erlang = op.expression_erlang.format(operand.expression_erlang)
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
                if node.id == 'CV_8U':
                    self.expression = '0'
                    self.expression_erlang = '0'
                elif node.id == 'CV_8S':
                    self.expression = '1'
                    self.expression_erlang = '1'
                elif node.id == 'CV_16U':
                    self.expression = '2'
                    self.expression_erlang = '2'
                elif node.id == 'CV_16S':
                    self.expression = '3'
                    self.expression_erlang = '3'
                elif node.id == 'CV_32S':
                    self.expression = '4'
                    self.expression_erlang = '4'
                elif node.id == 'CV_32F':
                    self.expression = '5'
                    self.expression_erlang = '5'
                elif node.id == 'CV_64F':
                    self.expression = '6'
                    self.expression_erlang = '6'
                elif node.id == 'CV_16F':
                    self.expression = '7'
                    self.expression_erlang = '7'
                elif node.id == 'CV_MAT_CONT_FLAG':
                    self.skip_this = True
                elif node.id == 'CV_SUBMAT_FLAG':
                    self.skip_this = True
                else:
                    print(type(node), node.id, "not handled yet")                    
                    sys.exit(1)
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
