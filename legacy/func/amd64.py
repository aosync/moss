class Location:
    def __init__(self):
        self.bound = False

class Reg(Location):
    def __init__(self, name):
        super().__init__()
        self.name = name
    
    def text(self):
        return '%%%s' % self.name

class Imem(Location):
    def __init__(self, base, index, scale, offset):
        super().__init__()
        self.base = base
        self.index = index
        self.scale = scale
        self.offset = offset
    
    def text(self):
        # This function is a stub
        return '%d(%s)' % (self.offset, self.base.text())

RAX = Reg('rax')
RBX = Reg('rbx')
RCX = Reg('rcx')
RDX = Reg('rdx')

class Amd64Bindings:
    def __init__(self):
        

class Amd64Gen:
    def __init__(self):
        self.regs = [
            rax, rbx, rcx, rdx
        ]
        
        self.rsp = Reg('rsp')
    
        self.stack = [
            Imem(self.rsp, 0, 0, 0)
        ]
    
        # Static-location association
        self.locs = {}
    
    
        # States-bindings association
        self.states = []
        
    def state(self, state):
        
        