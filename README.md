# Organização

- [x] Analisador Léxico [Victor <varm@ic.ufal.br>]
- [x] Analisador Sintático [Victor <varm@ic.ufal.br>]
- [x] Analisador Semântico [Renilson <renilson.123@hotmail.com>]
- [ ] Runner [José Gomes <jgsj@ic.ufal.br>]
- [ ] Relatório [Monique ]

## Como buildar e executar o projeto

```bash
make
./minipar <nome_do_arquivo>
```

A compilação do projeto gera um executável chamado `minipar`, que pode ser utilizado para compilar e executar o código fonte de um arquivo. O arquivo deve ser passado como argumento para o executável.

Também é possível compilar o projeto utilizando o CMake, através do CMakeLists.txt. Para isso, basta executar os seguintes comandos:

```bash
mkdir build
cd build 

cmake ..

cmake --build .
```
