# COSE212 - Programming Language, 2026 Fall
# 실습환경 안내: Local (Docker)

## HOW TO
### 1. Docker 설치
https://www.docker.com/

설치 확인: CLI에서 `docker --version` 실행 (버전은 달라도 됨)
```console
ta@cose212:~$ docker --version
Docker version 27.1.1, build 6312585
```

### 2. Visual Studio Code (VSC) 설치
https://code.visualstudio.com/

### 3. VSC Extension 'Dev Containers' 설치
![vsc_dev_containers](https://github.com/user-attachments/assets/ac0da2cb-cd10-4a23-8df5-0dd78d46be5b)

### 4. Dev container 실행
1. Clone this repository
    ```console
    ta@cose212:~$ git clone https://github.com/kupl-courses/COSE212-2026fall.git
    ```
    (필요시 git 다운로드: https://git-scm.com/downloads)
2. Open in VSC
    ```console
    ta@cose212:~$ code COSE212-2026fall
    ```
3. 우측 아래 메시지의 'Reopen in Container' 클릭

    ![vsc_reopen_in_container-1](https://github.com/user-attachments/assets/589d4a6c-ad83-4750-b7b2-e48c1651e198)
    메시지가 뜨지 않을 경우 좌측 아래의 'Open a Remote Window' 버튼 클릭 후 'Reopen in Container' 클릭
    ![vsc_reopen_in_container-2](https://github.com/user-attachments/assets/dfa43967-b0cc-4087-8d64-f2b2c58ecde4)

## 코드 실행
### REPL 사용
VSC 터미널에서 `ocaml` 실행
```console
$ ocaml
```
Tip) .ocamlinit 파일에 `#use "<파일명>.ml"`을 작성하면 REPL 실행시에 해당 파일을 자동으로 불러옴

### OCaml 스크립트 실행 (;;로 구분 필요)
VSC 터미널에서 `ocaml <파일명>.ml` 또는 `ocaml < <파일명>.ml` 실행
```console
$ ocaml < src/scratch_pad.ml
```

### .ml 파일 컴파일 및 실행 (;;로 구분 필요 없음)
VSC 터미널에서 `ocamlc -o <실행파일명> <파일명>.ml` 또는 `ocamlopt -o <실행파일명> <파일명>.ml` 실행
```console
$ ocamlopt -o main main.ml
$ ./main
```

## 업데이트된 과제 템플릿 다운로드
COSE212-2026fall 디렉토리에서 `git pull` 실행
```console
$ pwd
/workspaces/COSE212-2026fall
$ git pull
```