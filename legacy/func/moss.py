class Static:
    pass

class Variable(Static):
    pass

class Derv(Static):
    def __init__(self, var, state):
        self.var = var
        self.state = state

class State:
    def __init__(self, state=None):
        self.state = state
        
        # Input variables
        self.inv = state.outv if state else {}
        
        # Output variables
        self.outv = {}
        if state:
            for var in self.inv.keys():
                self.outv[var] = Derv(var, self)
    
    def __getitem__(self, var):
        if var in self.outv:
            return self.outv[var]
        else:
            return None
    
    def __setitem__(self, var, static):
        self.outv[var] = static
    
    def chk_funcall(self):
        for var in self.inv.values():
            if var.chk_funcall():
                return True
    
        return False

class If(State):
    def __init__(self, state, cond, yes, no):
        super().__init__(state)
        
        self.cond = cond
        self.yes = yes
        self.no = no

class Funcall(Static):
    def __init__(self, *args):
        self.args = args
        print(self.args)
    
    def chk_funcall(self):
        return True

class Imm(Static):
    def __init__(self, value):
        self.value = value
    
    def chk_funcall(self):
        return False

class Add(Static):
    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs
    
    def chk_funcall(self):
        return self.lhs.chk_funcall() and self.rhs.chk_funcall()

state = State()

a = Imm(0)
b = Imm(1)
s = Add(a, b)

state['myvar'] = s

#state['myvar'] = Funcall(state['myvar'])

state = State(state)

print(state.chk_funcall())