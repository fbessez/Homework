.class public C
.super java/lang/Object

.method public <init>()V
aload_0
invokenonvirtual java/lang/Object/<init>()V
return
.end method
.method public static main([Ljava/lang/String;)V
.limit locals 1000
.limit stack 1000
ldc 6
istore 0
iload 0
ldc 7
iadd 
istore 1
iload 1
invokestatic CSupport/printInt(I)V
ldc 4
istore 2
iload 2
invokestatic CSupport/printInt(I)V
iload 2
istore 0
iload 0
invokestatic CSupport/printInt(I)V
iload 0
invokestatic CSupport/printInt(I)V
iload 1
invokestatic CSupport/printInt(I)V
return
nop
.end method
