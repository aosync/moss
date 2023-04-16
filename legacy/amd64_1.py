class Location:
    def __init__(self):
        self.bound = 0

class RegLong(Location):
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

class Imm(Location):
    def __init__(self, value):
        super().__init__()
        self.value = value
    
    def text(self):
        return '$%d' % self.value

class Amd64Gen:
    def __init__(self):
        rax = RegLong('rax')
        rbx = RegLong('rbx')
        rcx = RegLong('rcx')
        rdx = RegLong('rdx')
        
        self.regs = [
            rax, rbx, rcx, rdx
        ]
        
        self.rsp = RegLong('rsp')
        
        # Set of all statics that have been genned yet
        self.gend = set()
        
        # Statics-location association
        self.locs = {}
        
        # Stack location
        self.stack = [
            Imem(self.rsp, 0, 0, 0)
        ]

    # STACK

    def stack_alloc(self, n):
        offset = self.stack[-1].offset - n
        loc = Imem(self.rsp, 0, 0, offset)
        
        # Append to the stack
        self.stack.append(loc)
        
        return loc
    
    def stack_free(self):
        while self.stack[-1].bound == 0:
            self.stack.pop()
    
    # REGISTERS
    
    def reg_alloc(self):
        for reg in self.regs:
            if reg.bound == 0:
                return reg
        
        return None

    # BINDING
    
    def bind(self, st, loc):
        self.locs[st] = loc
        loc.bound += 1

    def unbind(self, st):
        loc = self.locs[st]
        loc.bound -= 1
        del self.locs[st]
    
    # ALLOC STUFF

    def imm(self, st):
        imm = Imm(st.value)
        self.bind(st, imm)

    def load(self, st):
        self.bind(st, self.locs[st.var])

    def variable(self, st):
        loc = self.stack_alloc(4)
        
        self.bind(st, loc)

    # INTRINSICS

    def mark_gend(self, st):
        if st in self.gend:
            return True
        
        self.gend.add(st)
        
        return False

        
    # GEN

    def movl(self, d0, d1):
        print('\tmovl %s, %s' % (d0.text(), d1.text()))

    def addl(self, d0, d1):
        print('\taddl %s, %s' % (d0.text(), d1.text()))
    

    # INTRINSICS

    def mov(self, d0, d1):
        Amd64Gen.mov_intr[(type(d0), type(d1))](self, d0, d1)
    
    mov_intr = {
        (RegLong, RegLong): movl,
        (Imem, RegLong): movl,
        (RegLong, Imem): movl,
    }
    
    def sum_ulong(self, st):
        d0 = self.locs[st.lhs]
        d1 = self.locs[st.rhs]
        
        self.unbind(st.lhs)
        self.unbind(st.rhs)
        
        if type(d0) == Imem and type(d1) == Imem:
            good = False
        elif type(d1) == Imm:
            good = False
        else:
            good = True
            
        if not good or d1.bound > 0:
            dd = self.reg_alloc()
            self.movl(d1, dd)
        else:
            dd = d1
        self.bind(st, dd)
        
        self.addl(d0, dd)

    add_intr = {
        (RegLong, RegLong): addl,
        (Imem, RegLong): addl,
        (RegLong, Imem): addl,
    }
