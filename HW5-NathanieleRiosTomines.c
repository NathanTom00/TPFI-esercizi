#include <stdio.h>
#include <stdlib.h>

//Esercizio 1
int contaDivisori (int num){
    int counter = 0;
    for(int i=1; i<=num; i++)
        if(num%i==0)
            counter++;
    return counter;
}


//metodo per trovare il numero di divisori di d[i] (e salva il dato in d[i])
void calcoloDivisori (int d[], int k){
    for (int i=0 ; i<k ; i++){
        d[i] = contaDivisori(i);
    }
    
}



//metodo per trovare il primo numero che ha n divisori
int altamenteComposto (int counter,int k){
    for (int i=0 ; i<k ; i++)
        if(contaDivisori(i) == counter)
            return i;
    
    return -1;

}

int* maxAltamenteComposto(int k, int* ac){
    /* PREC: k>0, POST: torna un vettore d lungo k
    * tale che d[i]=numero divisori di i>0 e carica in *ac
    * il maggiore numero altamente composto < k.
    */
    int* d = (int*) calloc (k,sizeof(int));
    calcoloDivisori(d,k);
    int x = altamenteComposto(d[k-1],k);
    ac = &x;
    return d;
}


//Es 2
//ritorna 0 se non è ulam e uno altrimenti
int isUlam(int ulams[],int len,int next){
    int count=0;
    for(int i=0;i<len-1;i++){
        for(int j=i+1;j<len;j++){
            if(ulams[i] + ulams[j] == next){
                count++;
            }
            if(count>=2)
            {
                return 0;
            }
        }
    }
    return 1;
}

//creo il vettore di n elementi ulams
void calcoloUlams(int ulams[],int n){
    ulams[0] = 1;
    ulams[1] = 2;
    int nextUlam = 3;
    int len = 2;

    int i=2;
    while(i<n){
        if(isUlam(ulams,len,nextUlam)==1){
            len++;
            ulams[i++]=nextUlam;
        }
        nextUlam++;
    }
}

int ulam(int n){
    int* d = (int*) calloc (n,sizeof(int));
    calcoloUlams(d,n);

    return d[n-1];
}


//Es 3


typedef struct L
{
    int val;
    struct L* next;
    struct L* prev;
    
}nodo ;
typedef nodo* puntNodo;

//costruttori di nodo*
puntNodo emptyPuntNodo(){
    return NULL;
}

puntNodo consPuntoNodo(int x , puntNodo L){
    puntNodo M = (puntNodo) malloc (sizeof(nodo));
    M->val = x;
    if(L)
        L->prev = M;
    M->next = L;
    M->prev = emptyPuntNodo();

    return M;
}

typedef struct D {

    puntNodo first;
    puntNodo last;

} list;



typedef list* listDCFirstLast;

listDCFirstLast emptyList(){
    return NULL;
}

listDCFirstLast consList(int x,listDCFirstLast L){
    listDCFirstLast M = (listDCFirstLast) malloc (sizeof(list));
    
    puntNodo N;
    if(!L){
        N = consPuntoNodo(x,NULL);
        M->last = N;
    }
    else{
        N = consPuntoNodo(x,L->first);
        M->last = L->last;
    }

    M->first = N;
    
}

listDCFirstLast add (int x,listDCFirstLast L ){
    if (!L) return consList(x,NULL);

    puntNodo N= consPuntoNodo(x,NULL);
    N->prev = L->last;
    L->last->next = N;
    L->last = N;

    return L;
}

int len(listDCFirstLast L){
    if(!L->first)
        return 0;
    
    listDCFirstLast LAux=(listDCFirstLast) malloc (sizeof(list));
    LAux->first = L->first->next;
    LAux->last = L->last;
    return 1 + len(LAux);
}

/*int* toVec(listDCFirstLast L){
    if(!L)
        return NULL;

    listDCFirstLast LAux=(listDCFirstLast) malloc (sizeof(list));
    LAux->first = L->first;
    LAux->last = L->last;
    int n=len(L);
    int* ris = (int*) malloc (n*sizeof(int));
    for(int i=0;i<n;i++){
        ris[i] = LAux->first->val;
        LAux->first = LAux->first->next;
    }

    return ris;

}*/

int nextU(listDCFirstLast U){
    int k = len(U);
    return ulam(k+2);
}


/*
Esercizio 5
*/

typedef struct C {
    int pos;
    int succ;
    int prec;
} Pair;

Pair* iniz(int n){
    Pair* v = calloc (n-1,sizeof(Pair));

    for(int i=0;i<n-1;i++){
        v[i].pos = i+2;
        v[i].succ = 1;
        v[i].prec = 1;
    }

    return v;
    
}


void elimina (Pair* v,int index,int n){
    //v[index] è il pos da "eliminare"
    int iSucc,iPrec;
    if(index<n-1){
        //trovo il successivo e devo aggiornare il suo prec: sommo il prec di v[index] e il prec di v[iSucc]
        iSucc = index+v[index].succ;
        v[iSucc].prec += v[index].prec;
        

        if(index>=0){
            //stesso ragionamento ma inverso con il Pair prec a quello che vogliamo eliminare
            iPrec = index-v[index].prec;
            v[iPrec].succ += v[index].succ;

            
        }
    }    


}

Pair* eulerSieve(int n){
    Pair* v = iniz(n);
    int pick;
    int succ;
    //scorre di successivo in successivo per prendere i pick
    int i=0;
    int j=0;
    int* vecchieSucc = (int*) calloc (n-1,sizeof(int)); 
    while(i<n-1){
        pick = v[i].pos;
        if(v[i].pos*pick-2>n)
            break;

        for (int k = 0; k < n-1; k++)
        {
            vecchieSucc[k] = v[k].succ;
        }
        
        //scorre di successivo in successivo per rilevare quali sono i numeri da eliminare
        j=i;
        while(j<n-1){
            //prendo il numero v[j].pos, moltiplico * pick e elimino v[k] che ha pos = v[j].pos * pick
            //NOTA v[k].pos = k+2; ==> k=v[j].pos * pick - 2 ==>  devo eliminare v[v[j].pos*pick-2]


            //per evitare calcoli inutili quando ho v[j].pos*pick-2 > n faccio un break alla prima while
            if(v[j].pos*pick-2>n)
                break;
            else
                elimina(v,v[j].pos*pick-2,n);

            //se elimino un risultato devo aggiornare i succ e prec dei suoi vicini

            j += vecchieSucc[j];
        }
        

        i=i+v[i].succ;
    }

    return v;
}

void printPrimes(int n){
    Pair* d = eulerSieve(n);

    for(int i=0;i<n-1;){
        printf("%d  ",d[i].pos);
        i +=  d[i].succ;
    }

}



/*int main (int argc, char* argv[]) {
    printPrimes(100);
    



    return 0;
}*/

