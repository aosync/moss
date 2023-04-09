class Location:
    def __init__(self):
        self.bound = False

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
        while not self.stack[-1].bound:
            self.stack.pop()
    
    # REGISTERS
    
    def reg_alloc(self):
        for reg in self.regs:
            if not reg.bound:
                return reg
        
        return None

    # BINDING
    
    def bind(self, st, loc):
        self.locs[st] = loc
        loc.bound = True

    def unbind(self, st):
        loc = self.locs[st]
        loc.bound = False
        del self.locs[st]
    

    # GENERATION

    def mark_gend(self, st):
        if st in self.gend:
            return True
        
        self.gend.add(st)
        
        return False

    def mov_im_rl(self, d0, d1):
        print('\tmovl %s, %s' % (d0.text(), d1.text()))

    def mov(self, d0, d1):
        Amd64Gen.mov_intr[(type(d0), type(d1))](self, d0, d1)

    mov_intr = {
        (Imem, RegLong): mov_im_rl,
    }

    def add_im_im(self, d0, d1):
        print('\taddl %s, %s' % (d0.text(), d1.text()))

    def add_rl_rl(self, d0, d1):
        print('\taddl %s, %s' % (d0.text(), d1.text()))
    
    def add(self, st):
        d0 = self.locs[st.lhs]
        d1 = self.locs[st.rhs]
        
        dd = self.reg_alloc()
        self.mov(d1, dd)
        self.bind(st, dd)
        
        Amd64Gen.add_intr[(type(d0), type(d1))](self, d0, dd)
    
    add_intr = {
        (RegLong, RegLong): add_rl_rl,
        (Imem, Imem): add_im_im,
    }
    
    def var(self, st):
        loc = self.stack_alloc(4)
        
        self.bind(st, loc)