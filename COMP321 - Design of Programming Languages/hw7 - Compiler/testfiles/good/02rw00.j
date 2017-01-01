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
invokestatic CSupport/readInt()I
istore 0
iload 0
invokestatic CSupport/printInt(I)V
invokestatic CSupport/readDouble()D
dstore 1
dload 1
invokestatic CSupport/printDouble(D)V
invokestatic CSupport/readBool()Z
istore 3
iload 3
invokestatic CSupport/printBool(Z)V
