.intel_syntax noprefix

.text

i1: mov rax, rbx

i2: mov r12, QWORD PTR [rax + 2*rbx + 0x112233]

i3: mov r12, QWORD PTR [rax + 2*rbx + 0x112233]

i4: mov rax, 0x11223344556677

i5: mov rax, 13

i6: mov rax, dr9

i7: mov rbx, QWORD PTR [-0x112233]

i8: mov rbx, QWORD PTR [0x112233]
