def gen(pyfunc):
    def wrapper(self, gen):
        if gen.mark_gend(self):
            return
        
        return pyfunc(self, gen)
    
    return wrapper


class Type:
    pass

ULONG = Type()

# STATICS

class Static:
    def __init__(self, atype):
        self.type = atype

class Variable(Static):
    def __init__(self, atype):
        super().__init__(atype)
    
    @gen
    def gen(self, gen):
        gen.variable(self)

class Load(Static):
    def __init__(self, var):
        super().__init__(var.type)
        
        self.var = var
    
    @gen
    def gen(self, gen):
        self.var.gen(gen)
    
        gen.load(self)

class Imm(Static):
    def __init__(self, value):
        super().__init__(ULONG)
        
        self.value = value
    
    @gen
    def gen(self, gen):
        gen.imm(self)

class Sum(Static):
    def __init__(self, lhs, rhs):
        super().__init__(lhs.type)
    
        self.lhs = lhs
        self.rhs = rhs
    
    @gen
    def gen(self, gen):
        self.lhs.gen(gen)
        self.rhs.gen(gen)
        
        if self.type == ULONG:
            gen.sum_ulong(self)
        else:
            print('error')

class Assign(Static):
    def __init__(self, var, static):
        super().__init__(static.type)
    
        self.var = var
        self.static = static
    
    @gen
    def gen(self, gen):
        self.var.gen(gen)
        self.static.gen(gen)
        
import amd64

gen = amd64.Amd64Gen()

a = Variable(ULONG)
b = Variable(ULONG)
s = Sum(Load(a), Load(b))
s = Sum(Imm(4), s)

s.gen(gen)