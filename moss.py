class Static:
    def __init__(self):
        self.birth = 0
        self.death = 0
    
    def pass_reset_liveness(self):
        self.birth = 0
        self.death = 0
    
    def pass_determine_liveness(self):
        self.pass_reset_liveness()
        self.pass_determine_birth()
        self.pass_determine_death()
    
    def pass_determine_birth(self, epoch=1):
        if self.birth == 0:
            self.birth = epoch
            epoch += 1
        
        return epoch
    
    def pass_determine_death(self, epoch=0):
        if epoch == 0:
            epoch = self.birth
    
        self.death = max(self.death, epoch)
    
    def pass_relax_order(self):
        # XXX temporary
        return False
    
    def pass_link_loci(self):
        self.pass_constraint_loci()
        self.pass_replace_loci()
        
        # XXX wanted?
        self.pass_determine_liveness()
    
    def pass_constraint_loci(self):
        pass
    
    def pass_replace_loci(self):
        pass

class Imperator(Static):
    def __init__(self):
        super().__init__()
    
        self.scope = None
        self.scopes = []
        self.statics = []
    
    def begin_scope(self):
        self.scope = Scope()
        
        self.scopes.append(self.scope)

        return self.scope
    
    def end_scope(self):
        self.add(self.scope)
        
        self.scopes.pop()
        
        if len(self.scopes) > 0:
            self.scope = self.scopes[-1]
        else:
            self.scope = None
    
    def add(self, static):
        self.statics.append(static)

        return static
    
    def datum(self, datum):
        if self.scope:
            self.scope.add(datum.locus)
        
        return self.add(datum)
    
    def pass_reset_liveness(self):
        super().pass_reset_liveness()
        
        for static in self.statics:
            static.pass_reset_liveness()
    
    def pass_determine_birth(self, epoch=1):
        for static in self.statics:
            epoch = static.pass_determine_birth(epoch)
        
        return super().pass_determine_birth(epoch)
    
    def pass_determine_death(self, epoch=0):
        super().pass_determine_death(epoch)
        
        for static in self.statics:
            static.pass_determine_death(static.birth)
    
    def pass_relax_order(self):
        volatile = False
        new_statics = []
        
        for static in self.statics:
            if static.pass_relax_order():
                volatile = True
                new_statics.append(static)
        
        self.statics = new_statics
        
        return volatile
    
    def pass_constraint_loci(self):
        for static in self.statics:
            static.pass_constraint_loci()

    def pass_replace_loci(self):
        for static in self.statics:
            static.pass_replace_loci()

class Scope(Static):
    def __init__(self):
        super().__init__()
        
        self.loci = set()
    
    def add(self, locus):
        self.loci.add(locus)
    
    def pass_determine_death(self, epoch=0):
        super().pass_determine_death(epoch)
        
        for locus in self.loci:
            if locus.leaked:
                locus.pass_determine_death(epoch)
    
    def pass_relax_order(self):
        for locus in self.loci:
            if locus.leaked:
                return True
        
        return False

    def pass_replace_loci(self):    
        trashed = [locus for locus in self.loci if locus.replace_by]
    
        for locus in trashed:
            self.loci.remove(trashed)
                

class Locus(Static):
    def __init__(self):
        super().__init__()
        
        # Locus is addressed?
        self.addressed = False
        
        # Locus is leaked?
        self.leaked = False
        
        # Locus is constrained?
        self.constrained = False
        
        # Locus should be replaced by
        self.replace_by = None
    
    def deepest_replacement(self):
        replacement = self
        
        while replacement.replace_by:
            replacement = replacement.replace_by
        
        return replacement

class Datum(Static):
    def __init__(self):
        super().__init__()
        
        self.locus = Locus()
    
    def children(self):
        return ()
    
    def pass_reset_liveness(self):
        super().pass_reset_liveness()
        
        # Reset locus livenesss
        self.locus.pass_reset_liveness()
        
        # Reset children liveness
        for child in self.children():
            child.pass_reset_liveness()
    
    def pass_determine_birth(self, epoch=1):
        # Then children birth
        for child in self.children():
            epoch = child.pass_determine_birth(epoch)
    
        # Locus birth goes before self (it needs to outlive)
        epoch = self.locus.pass_determine_birth(epoch)
        
        return super().pass_determine_birth(epoch)
    
    def pass_determine_death(self, epoch=0):
        super().pass_determine_death(epoch)
        
        # Locus needs to outlive datum
        self.locus.pass_determine_death(self.death)
        
        # Children need to live until at least birth of datum
        for child in self.children():
            child.pass_determine_death(self.birth)

    def pass_constraint_loci(self):
        for child in self.children():
            child.pass_constraint_loci()
    
    def pass_replace_loci(self):
        replacement = self.locus.deepest_replacement()
    
        if replacement != self.locus:
            self.locus = replacement
        
        for child in self.children():
            child.pass_replace_loci()

class Imm(Datum):
    def __init__(self, value):
        super().__init__()

        self.value = value

class Biop(Datum):
    def __init__(self, lhs, rhs):
        super().__init__()
    
        self.lhs = lhs
        self.rhs = rhs
    
    def children(self):
        return (self.lhs, self.rhs)
   
class Add(Biop):
    def __init__(self, lhs, rhs):
        super().__init__(lhs, rhs)
    def pass_relax_order(self):
        return True

class Phi(Datum):
    def __init__(self, *xi):
        super().__init__()
        self.xi = tuple(xi)
        self.weak_xi = []
    
    def weak(self, xi):
        self.weak_xi.append(xi)
    
    def pass_constraint_loci(self):
        locus = self.xi[0].locus
        
        for child in self.xi[1:]:
            child.locus.deepest_replacement().replace_by = locus
        
        for child in self.weak_xi:
            child.locus.deepest_replacement().replace_by = locus
    
    def children(self):
        return self.xi

#imp = Imperator()
#imp.begin_scope()

#b = imp.datum(Imm(2))
#a = imp.datum(Imm(1))
#add = imp.datum(Add(a, b))

#addd = imp.datum(Add(add, a))
#imp.end_scope()

#imp.pass_relax_order()
#imp.pass_determine_liveness()

# a = Imm(1)
# b = Imm(2)
# c = Phi(a, b)
# add = Add(a, c)
# addd = Add(add, b)

# addd.pass_determine_liveness()
# addd.pass_link_loci()

a = Imm(1)
b = Imm(2)

add = Add(a, b)

add.pass_determine_liveness()
add.pass_link_loci()

print('a.locus', a.locus.birth, a.locus.death)
print('a', a.birth, a.death)
print('b.locus', b.locus.birth, b.locus.death)
print('b', b.birth, b.death)
print('add.locus', add.locus.birth, add.locus.death)
print('add', add.birth, add.death)
# print('addd.locus', addd.locus.birth, addd.locus.death)
# print('addd', addd.birth, addd.death)