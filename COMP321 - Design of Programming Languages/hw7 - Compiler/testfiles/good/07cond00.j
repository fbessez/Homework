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
ldc 1
ldc 0
ldc 1
ifeq LBL0
goto LBL1
LBL0: 
LBL1: 
invokestatic CSupport/printInt(I)V
return
nop
.end method
