class Type:
    pass

class Static:
    def __init__(self, mtype):
        self.type = mtype

    def typechk(self):
        return True

class Variable(Static):
    def __init__(self, mtype):
        self.type = mtype
    
    def gen(self, gen):
        if gen.mark_gend(self):
            return
    
        gen.var(self)

class Add(Static):
    def __init__(self, lhs, rhs):
        super().__init__(lhs.type)

        self.lhs = lhs
        self.rhs = rhs

    def typechk(self):
        self.lhs.typechk()
        self.rhs.typechk()
        
        return True

    def gen(self, gen):
        if gen.mark_gend(self):
            return
        
        self.lhs.gen(gen)
        self.rhs.gen(gen)
    
        gen.add(self)

import amd64

gen = amd64.Amd64Gen()

v1 = Variable(None)
v2 = Variable(None)

s = Add(v1, v2)

s.gen(gen)