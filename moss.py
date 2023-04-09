class Type:
    pass

class Static:
    def __init__(self, mtype):
        self.type = mtype
        self.last_use = self

    def typechk(self):
        return True
    
    def chk(self):
        self.last_use = self

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

    def chk(self):
        self.lhs.chk()
        self.rhs.chk()
        
        self.lhs.last_use = self
        self.rhs.last_use = self

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

a = Variable(None)
b = Variable(None)
s = Add(a, b)

gen = amd64.Amd64Gen()
s.gen(gen)