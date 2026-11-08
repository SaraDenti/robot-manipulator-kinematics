(* ::Package:: *)

(*Questa procedura restituisce True se x ha una parte complessa*)
parteComplessa[x_]:=(FullSimplify[Refine[ComplexExpand[Im[x]]]]=!=0)

(*Questa procedura restituisce True se \[Theta] \[EGrave] un numero reale (anche simbolico)*)
numeroReale[\[Theta]_]:= !ListQ[\[Theta]]&& !StringQ[\[Theta]] && !parteComplessa[\[Theta]]

(*Questa procedura restituisce True se il vettore passato in ingresso \[EGrave] reale, False altrimenti. Non controlla gli argomenti passati in ingresso*)
vettoreReale[v_,dim_]:=Module[{i,found},
	i = 1;
	found = False;(*Variabile che indica se c'\[EGrave] un argomento complesso*)
	While[!found && i<=dim,
		If[numeroReale[v[[i]]], 
			i= i+1,
			found = True;
		];
	];
	!found
]
	
(*Questa procedura calcola la norma di v*)
norma[v_]:= Module[{norm}, 
	If[VectorQ[v], 
		norm = v . v//Sqrt;
		FullSimplify[norm], 
	
		Echo[False, "L'argomento passato non \[EGrave] un vettore"]
	]
]

(*Questa procedura verifica se l'argomento passato \[EGrave] un versore 3x3 a coefficienti reali*)
versore3x3[v_]:=VectorQ[v] && Dimensions[v]==={3} && vettoreReale[v, 3] && FullSimplify[norma[v]]===1 

(*Questa procedura prende come argomento un vettore e restituisce il versore ad esso associato. 
Non verifica direttamente se l'argomento passato \[EGrave] un vettore, ma lascia tale compito alla funzione "norma" chiamata al suo interno*)
versore[v_]:= Module[{n}, 
	n= norma[v];
	Which[n === False, n,
		n===0,Echo[v, "Il vettore passato \[EGrave] nullo"],
		True, FullSimplify[v/n]
	]
]

(*Questa procedura prende in ingresso un numero reale \[Theta] e restituisce la matrice Subscript[R, x](\[Theta])*)
matriceRx[\[Theta]_ ]:= If[numeroReale[\[Theta]],
	{{1, 0, 0}, {0, Cos[\[Theta]], -Sin[\[Theta]]}, {0, Sin[\[Theta]], Cos[\[Theta]]}}//FullSimplify,
	Echo[False, "L'argomento passato non \[EGrave] un angolo di rotazione valido"]
]
	
(*Questa procedura prende in ingresso un numero reale \[Theta] e restituisce la matrice Subscript[R, y](\[Theta])*)
matriceRy[\[Theta]_]:= If[numeroReale[\[Theta]],
	{{Cos[\[Theta]], 0, Sin[\[Theta]]}, {0, 1, 0}, {-Sin[\[Theta]], 0, Cos[\[Theta]]}}//FullSimplify,
	Echo[False, "L'argomento passato non \[EGrave] un angolo di rotazione valido"]
]

(*Questa procedura prende in ingresso un numero reale \[Theta] e restituisce la matrice Subscript[R, z](\[Theta])*)
matriceRz[\[Theta]_]:= If[numeroReale[\[Theta]],
	{{Cos[\[Theta]], -Sin[\[Theta]], 0}, {Sin[\[Theta]], Cos[\[Theta]], 0},{0, 0, 1}}//FullSimplify,
	Echo[False, "L'argomento passato non \[EGrave] un angolo di rotazione valido"]
]

(*Questa procedura prende in ingresso una lettera h="x","y","z" e un numero reale \[Theta] e restituisce Subscript[R, h](\[Theta])*)
matriceR[ h_,\[Theta]_] := If[numeroReale[\[Theta]], 
	Which[ h=== "x", matriceRx[\[Theta]],
		h ==="y", matriceRy[\[Theta]], 
		h ==="z", matriceRz[\[Theta]],
		True, Echo[False, "Non \[EGrave] stato selezionato un asse di rotazione valido"]
	],
	Echo[False, "Non \[EGrave] stato passato un angolo di rotazione valido"]
]

(*Questa procedura prende in ingresso una matrice R e verifica se \[EGrave] di rotazione*)
isMatrixRotation[r_] :=Module[{dim}, 
	If[MatrixQ[r]&&(Dimensions[r][[1]]===Dimensions[r][[2]]) && vettoreReale[Flatten[r], Dimensions[r][[1]]^2], 
		(*Verifica se \[EGrave] una matrice di rotazione*)
		dim =Dimensions[r][[1]];
		(FullSimplify[r . Transpose[r]- IdentityMatrix[dim]] ===  ConstantArray[0,{dim,dim}]) && (FullSimplify[Det[r]]===1), 

		Echo[False, "L'argomento passato non \[EGrave] una matrice nxn a coefficienti reali"]
	]
]

(*Questa procedura prende in ingresso una matrice di rotazione 3x3 e restituisce gli angoli alpha, beta, gamma relativi alla codifica nautica x,y,z*)
codificaNauticaXYZ[r_]:=Module[{alpha1,alpha2, beta1,beta2, gamma1, gamma2},
	If[isMatrixRotation[r] && Dimensions[r][[1]]===3,
		(*Verifica se \[EGrave] presente una singolarit\[AGrave]*)
		If[(FullSimplify[r[[1,3]]-1]===0 ||FullSimplify[r[[1,3]]+1]===0),

			Echo["E' presente una singolarit\[AGrave]"];
			{},

			(*Non \[EGrave] presente una singolarit\[AGrave]*)
			beta1 = ArcTan[Sqrt[1-r[[1,3]]^2], r[[1,3]]];
			beta2 = ArcTan[-Sqrt[1-r[[1,3]]^2], r[[1,3]]];

			(*Per ottenere un codice pi\[UGrave] efficiente, moltiplico per sign(Cos(beta)) invece di dividere per Cos[beta]*)
			alpha1= ArcTan[r[[3,3]], - r[[2,3]]];
			alpha2= ArcTan[-r[[3,3]], r[[2,3]]] ;

			gamma1= ArcTan[r[[1,1]], -r[[1,2]]];
			gamma2= ArcTan[-r[[1,1]], r[[1,2]]];


			{{alpha1, beta1, gamma1}, {alpha2, beta2, gamma2}}
		], 

		Echo[False, "L'argomento passato non \[EGrave] una matrice di rotazione reale"]
	]
]

(*Questa procedura prende in ingresso una matrice di rotazione 3x3 e restituisce gli angoli alpha, beta, gamma relativi alla codifica di Eulero x,y,x*)
codificaEuleroXYX[r_]:=Module[{alpha1,alpha2, beta1,beta2, gamma1, gamma2},
	If[isMatrixRotation[r] && Dimensions[r][[1]]===3,
		(*Verifica se \[EGrave] presente una singolarit\[AGrave]*)
		If[(FullSimplify[r[[1,1]]-1]===0 ||FullSimplify[r[[1,1]]+1]===0),

			Echo["E' presente una singolarit\[AGrave]"];
			{},

			(*Non \[EGrave] presente una singolarit\[AGrave]*)
			beta1 = ArcTan[r[[1,1]], Sqrt[1-r[[1,1]]^2]];
			beta2 = ArcTan[r[[1,1]],-Sqrt[1-r[[1,1]]^2]];

			(*Per ottenere un codice pi\[UGrave] efficiente, moltiplico per sign(Sin(beta)) invece di dividere per Sin[beta]*)
			alpha1= ArcTan[-r[[3,1]],  r[[2,1]]];
			alpha2= ArcTan[r[[3,1]], -r[[2,1]]] ;

			gamma1= ArcTan[r[[1,3]], r[[1,2]]];
			gamma2= ArcTan[-r[[1,3]], -r[[1,2]]];

			{{alpha1, beta1, gamma1}, {alpha2, beta2, gamma2}}
		], 

		Echo[False, "L'argomento passato non \[EGrave] una matrice di rotazione reale"]
	]
]

(*Questa procedura prende in ingresso un vettore e restituisce la matrice antisimmetrica 3x3 ad esso associata*)
matriceAntisimmetrica[v_]:= Module[{S},
	If[VectorQ[v] && Dimensions[v]==={3},
		S={{0, -v[[3]], v[[2]]}, {v[[3]], 0, -v[[1]]}, {-v[[2]], v[[1]], 0}},
		Echo[False, "L'argomento passato non \[EGrave] un vettore con 3 componenti"]
	]
]

(*Questa procedura prende in ingresso un versore v e un angolo \[Theta] e restituisce la matrice di rotazione ad essi associata*)
rotazioneLaplace[v_, \[Theta]_] := Module[{S, A,s,t, sol},
	If[versore3x3[v],
		If[numeroReale[\[Theta]], 
  
			(*Calcola la  matrice antisimmetrica associata a v. Da notare che la chiamata a tale funzione non restituir\[AGrave] mai False poich\[EGrave] i controlli su v sono gi\[AGrave] stati fatti*)
			S = matriceAntisimmetrica[v]; 
 
			(*Calcolo A=(sI-S)^-1*)
			A=s*IdentityMatrix[3]-S; 
			If[Det[A]===0,
				Echo["La matrice sI-S(v) non \[EGrave] invertibile"];
				Abort[]
			];
			A=Inverse[A];

			(*Calcola matrice di rotazione Subscript[R, u](\[Theta])*)
			sol = InverseLaplaceTransform[A,s,t]//FullSimplify;
			t=\[Theta];
			sol,
			
			Echo[False, "Il secondo argomento non \[EGrave] un angolo"]
		],

		Echo[False, "Il primo argomento non \[EGrave] un versore reale formato da tre componenti"]
	]
]

(*Questa procedura implementa la formula di Rodriguez*)
rodriguez[v_, \[Theta]_]:= Module[{S}, 
	If[versore3x3[v],
		If[ numeroReale[\[Theta]],

			S=matriceAntisimmetrica[v];
			(1-Cos[\[Theta]])*S . S+Sin[\[Theta]]*S+IdentityMatrix[3]//FullSimplify,

			Echo[False, "Il secondo argomento non \[EGrave] un angolo"]
		], 
		
		Echo[False, "Il primo argomento non \[EGrave] un versore reale formato da 3 componenti"]
	]
]

(*Questa procedura restituisce la prima riga non nulla della matrice A 3x3. Non effettua alcun controllo riguardo la matrice passata in ingresso*)
primaRiga[A_]:= Module[{i, found},
	i = 1;
	found = False;
	While[!found && i<=3,
		If[A[[i]]==={0,0,0}, 
			i= i+1,
			found = True]
	];
	If [found === True,
		i,
		False
	]
]

(*Questa procedura ritorna la prima componente non nulla di un vettore riga con 3 componenti. Non \[EGrave] previsto un controllo sull'argomento passato*)
primaComponente[v_]:= Module[{i, found},
	i = 1;
	found = False;
	While[!found && i<=3,
		If[v[[i]]===0, 
			i= i+1,
			found = True
		];
	];
	If[found ===True,
		i, 
		False
	]
]

(*Questa procedura calcola l'asse e l'angolo associati alla matrice di rotazione r usando la formula di Rodriguez*)
rodriguezInversa[r_]:= Module[{A, i, v, M1, M2, found, j, h, k,sin, cos,  S, \[Theta]},
	If[MatrixQ[r] && Dimensions[r][[1]]===3&&isMatrixRotation[r],
		If [FullSimplify[r-IdentityMatrix[3]]===ConstantArray[0,{3,3}],
			(*La matrice passata come argomento \[EGrave] la matrice identit\[AGrave]*)
			{{1,0,0}, 0},

			(*La matrice passata non \[EGrave] la matrice identit\[AGrave]*)

			(*Calcolo dell'asse v*)
			A=Adjugate[r-IdentityMatrix[3]] //FullSimplify;
			(*Siccome la matrice identit\[AGrave] non \[EGrave] inclusa, A presenta almeno una colonna non nulla*)
			i = primaRiga[Transpose[A]];
			v = versore[{A[[1, i]], A[[2, i]], A[[3, i]]}];

			(*Calcolo dell'angolo \[Theta]*)
			M1 = (r-Transpose[r])/2 //FullSimplify;
			M2 = (r+Transpose[r])/2-IdentityMatrix[3]//FullSimplify;

			S = matriceAntisimmetrica[v]//FullSimplify;

			(*Determina il seno di \[Theta]*)
			i = primaRiga[M1];
			If[i ===False,
				sin = 0, 
				j =primaComponente[M1[[i]]];
				sin = M1[[i,j]]/S[[i,j]]
			];


			(*Determina il coseno di \[Theta]*)
			h = primaRiga[M2];
			If[h===False, 
				cos=0, 
				k =primaComponente[M2[[h]]];
				cos = 1-M2[[h,k]]/(S . S)[[h,k]];
			];

			\[Theta] = FullSimplify[ArcTan[cos, sin]];
			{{v, \[Theta]}, {-v, -\[Theta]}}
		], 

		Echo[False, "L'argomento passato non \[EGrave] una matrice di rotazione 3x3 reale"]
	]
]

(*Questa procedura prende in ingresso un vettore reale w e restituisce la matrice di rotazione associata alla matrice antisimmetrica S(w) usando la paramterizzazione di Cayley R=(I-S)^-1(I+S)*)
cayley[w_]:=Module[{S, r},
	If[VectorQ[w] && Dimensions[w]==={3} && vettoreReale[w,3],
		S=matriceAntisimmetrica[w];
		(*Siccome det(I-S)=x^2+y^2+z^2+1, det(I-S)!=0 se W ha coefficienti reali*)
		r=Inverse[IdentityMatrix[3]-S] . (IdentityMatrix[3]+S)//FullSimplify, 

		Echo[False, "L'argomento passato non \[EGrave] un vettore reale formato da tre componenti"]
	]
]

(*Questa procedura prende in ingresso la matrice di rotazione R 3x3 e restituisce l'asse e l'angolo di rotazione ad essa associati utilizzando la parametrizzazione di Cayley S=(R-I)(R+I)^-1*)
cayleyInversa[r_]:=Module[{S,w, norm, v, \[Theta]},
	If[MatrixQ[r]&&Dimensions[r][[1]]===3 && isMatrixRotation[r], 

		(*Verifica di non essere in singolarit\[AGrave]*)
		If[FullSimplify[Det[r+IdentityMatrix[3]]]=!=0, 

			(*Calcola il vettore w associato alla matrice di rotazione*)
			S=(r-IdentityMatrix[3]) . Inverse[r+IdentityMatrix[3]];
			w={S[[3,2]],S[[1,3]],S[[2,1]]}//FullSimplify;

			(*Calcola l'asse e l'angolo di rotazione*)
			norm= norma[w];
			If[norm===0,
				(*Se la matrice di rotazione \[EGrave] la matrice identit\[AGrave]*)
				Echo["Non \[EGrave] avvenuta alcuna rotazione attorno ad un asse"];
				{},

				(*Se la matrice di rotazione non \[EGrave] la matrice identit\[AGrave]*)
				v=FullSimplify[w/norm];
				\[Theta]=ArcTan[1-norm^2, 2norm]//FullSimplify;
				{{v,\[Theta]},{-v,-\[Theta]}}
			],

			Echo["La matrice r+I non \[EGrave] invertibile"];
			{}
		],
		
		Echo[False, "L'argomento passato non \[EGrave] una matrice di rotazione 3x3 reale"]
	]
]

(*Questa procedura restituisce la matrice di avvitamento con rotazione attorno all'asse v di angolo \[Theta] e traslazione pari a dv*) 
matriceAvvitamento[v_, \[Theta]_, d_]:=Module[{m1, m2},
	If[versore3x3[v], 
		If[numeroReale[\[Theta]],
			If[numeroReale[d],

				ArrayFlatten[{{rodriguez[v,\[Theta]],d*Transpose[{v}]},{0,1}}],

				Echo[False, "Il terzo argomento passato non \[EGrave] un numero reale"]
			],
			Echo["Il secondo argomento passato non \[EGrave] un numero reale"]
		],
		Echo["Il primo argomento passato non \[EGrave] un versore reale"]
	]
]

(*Questa procedura prende in ingresso la tabella di Denavit-Hartemberg e restituisce la cinematica diretta del robot*)
cinematicaDiretta[t_]:=Module[{m, ex, ez},
	If[MatrixQ[t] && Dimensions[t][[2]]===4&&vettoreReale[Flatten[t], Dimensions[t][[1]]*Dimensions[t][[2]]],

		ex={1,0,0};
		ez={0,0,1};
		m = IdentityMatrix[4];
		Do[m =  FullSimplify[m . matriceAvvitamento[ez, t[[i,1]],t[[i,2]]] . matriceAvvitamento[ex, t[[i,3]],t[[i,4]]]],{i, 1, Dimensions[t][[1]]}];
		m,
		
		Echo[False, "L'argomento passato non \[EGrave] una tabella di Denavith-Artenberg a componenti reali"]
	]
]

(*Questa procedura prende in ingresso:
 - la traiettoria {xx3,yy3,zz3} che il robot deve inseguire (in funzione del tempo t)
 - la traiettoria reale che il robot pu\[OGrave] inseguire (in funzione delle variabili di giunto q1,q2 e q3)
 - la variabile da cui dipendono le traiettorie (che dev'essere pari alla variabile globale t)
 - la stima iniziale delle variabili di giunto (q10,q20,q30)
 - il tempo finale di osservazione tf
 - il metodo che si deve adottare per inseguire la traiettoria desiderata(metodo di Newton: o="1",metodo del gradiente: o="2"; metodo misto: o="3)
  - il parametro \[Lambda], che indica la velocit\[AGrave] di convergenza dell'osservatore (pi\[UGrave] \[Lambda] \[EGrave] grande pi\[UGrave] veloce \[EGrave] l'osservatore a convergere)
  - il parametro \[Mu] del metodo misto (pi\[UGrave] \[Mu] \[EGrave] piccolo pi\[UGrave] il metodo si avvicina al metodo di Newton, pi\[UGrave] \[Mu] \[EGrave] grande pi\[UGrave] si avvicina al metodo del gradiente)
  - il titolo del primo plot s1 
   - il titolo del secondo plot s2. *)
(*Essa restituisce due diversi plot: il primo plot rappresenta il valore delle variabili di giunto nell'intervallo [0,tf], mentre il secondo le coordinate dell'end-effector in tale intervallo. 
NB: tale procedura NON controlla se traIdeale dipende unicamente dalla variabile t e se traReale dipende unicamente da varReale*)
osservatore[traIdeale_,traReale_,t_,varReale_,condIn_,tf_, o_,\[Lambda]_,\[Mu]_, s1_,s2_]:=Module[{q1,q2,q3,e,J,f,eqs,sol,res1,pp,p1,p2,p3,res2},
	If[AllTrue[{traIdeale, traReale, varReale,condIn}, MatchQ[{_, _, _}]] && vettoreReale[condIn,3] && AllTrue[{tf,\[Lambda],\[Mu]}, NumericQ[#]&&!parteComplessa[#]&&Positive[#]&]&&MemberQ[{"1","2","3"},o]&&AllTrue[{s1,s2},StringQ],

		(*Definizione delle variabili di giunto*)
		q1=varReale[[1]];
		q2=varReale[[2]];
		q3=varReale[[3]];
	
		(*Definizione dell'errore di stima*)
		e=traIdeale-traReale;

		(*Definizione delle equazioni differenziali da risolvere*)
		J=D[traReale,{{q1,q2,q3}}];
		Which[o==="1", K=Inverse[J]//FullSimplify;,
			o==="2",K=Transpose[J]//FullSimplify;,
			o==="3",K=Inverse[Transpose[J] . J+\[Mu] IdentityMatrix[3]] . Transpose[J]//FullSimplify;
		];
		f=1/2 \[Lambda] K . e/.{q1->q1[t],q2->q2[t],q3->q3[t]}//FullSimplify;
		eqs={q1'[t]==f[[1]],q2'[t]==f[[2]],q3'[t]==f[[3]],q1[0]==condIn[[1]],q2[0]==condIn[[2]],q3[0]==condIn[[3]]};

		(*Risoluzione delle equazioni differenziali*)
		sol = NDSolve[eqs,{q1,q2,q3},{t,0,tf}];

		(*Plot delle variabili*)
		res1=Plot[Evaluate[{q1[t],q2[t],q3[t]}/.sol[[1]]],{t,0,tf},PlotLegends->{"\!\(\*SubscriptBox[\(q\), \(1\)]\)[t]","\!\(\*SubscriptBox[\(q\), \(2\)]\)[t]","\!\(\*SubscriptBox[\(q\), \(3\)]\)[t]"},PlotRange->All,PlotLabel->s1];

		(*Plot delle traiettorie ideali e reali del robot*)
		pp=Evaluate[Evaluate[traReale/.{q1->q1[t],q2->q2[t],q3->q3[t]}]/.sol[[1]]];

		p1=Plot[{traIdeale[[1]],pp[[1]]},{t,0,tf},PlotRange->All,PlotLegends->{"stima","posizione"},PlotLabel->"Coordinata x"];
		p2=Plot[{traIdeale[[2]],pp[[2]]},{t,0,tf},PlotRange->All,PlotLegends->{"stima","posizione"},PlotLabel->"Coordinata y"];
		p3=Plot[{traIdeale[[3]],pp[[3]]},{t,0,tf},PlotRange->All,PlotLegends->{"stima","posizione"},PlotLabel->"Coordinata z"];

		res2=Labeled[GraphicsRow[{p1,p2,p3},Spacings->40],Style[s2,16],Top];

		{res1,res2}

	,Echo["Argomenti non validi"];
]]

(*Questa procedura prende in ingresso la matrice di rotazione R, la matrice delle inerzie II(diagonale) e la lista di 
variabili vars da cui dipende R. Essa restituisce la matrice delle inerzie generalizzate Subscript[B, \[Omega]] e l'energia cinetica di pura 
di rotazione Subscript[T, 1]*) 
energiaCineticaRot[R_,II_,vars_]:=Module[{varsT,varsD,varsDT,Rt,t,S,\[Omega],J\[Omega],B\[Omega],T1},
	If[isMatrixRotation[R]&&Dimensions[R]==={3,3},
		If[MatrixQ[II]&&Dimensions[II]=={3,3}&&DiagonalMatrixQ[II]&&vettoreReale[Flatten[II],9],
			If[VectorQ[vars]&&AllTrue[vars,numeroReale[#]&&!NumericQ[#]&],

				varsT=((#[t])&/@vars);
				varsD=(#')&/@vars;
				varsDT=((#[t])&/@varsD);
				Rt=R/. Thread[vars->varsT];

				(*Calcolo matrice antisimmetrica S=R'R^T*)
				S=D[Rt,t] . Transpose[Rt]//FullSimplify;

				(*Calcolo vettore velocit\[AGrave] angolari \[Omega]*)
				\[Omega]={S[[3,2]],S[[1,3]],S[[2,1]]}/. Join[Thread[varsT->vars],Thread[varsDT->varsD]];

				(*Calcolo Jacobiano *)
				J\[Omega]=D[\[Omega],{varsD}]//FullSimplify;

				(*Calcolo matrice delle inerzie generalizzate relative al moto di pura rotazione Subscript[B, \[Omega]]*)
				B\[Omega]=Transpose[J\[Omega]] . R . II . Transpose[R] . J\[Omega]//FullSimplify;

				(*Calcolo energia cinetica relativa al moto di pura rotazione*)
				T1=1/2Transpose[varsD] . B\[Omega] . varsD//FullSimplify;
				{B\[Omega],T1}
				,
				Echo["La lista vars deve contenere solo variabili simboliche"];],
			Echo["La matrice I passata non \[EGrave] una matrice diagonale 3x3"];],
		Echo["La matrice R passata non \[EGrave] una matrice di rotazione 3x3"];
	]
]

(*Questa procedura prende in ingresso la massa m di un corpo, il vettore di traslazione d e la lista di variabili vars da cui dipende d. 
Essa restituisce la matrice delle inerzie generalizzate Subscript[B, t] e l'energia cinetica di pura di traslazione Subscript[T, 2]*) 
energiaCineticaTrasl[m_,d_,vars_]:=Module[{varsD,Jt,Bt,T2},
	If[numeroReale[m]&&TrueQ[Refine[m>=0]],
		If[VectorQ[d]&&Dimensions[d]==={3}&&AllTrue[d,numeroReale[#]&],
			If[VectorQ[vars]&&AllTrue[vars,numeroReale[#]&&!NumericQ[#]&],

				varsD=(#')&/@vars;

				(*Calcolo Jacobiano*)
				Jt=D[d,{vars}]//FullSimplify;

				(*Calcolo matrice delle inerzie generalizzate relative al moto di pura traslazione Subscript[B, t]*)
				Bt=m Transpose[Jt] . Jt//FullSimplify;

				(*Calcolo energia cinetica relativa al moto di pura rotazione*)
				T2=1/2Transpose[varsD] . Bt . varsD//FullSimplify;
				{Bt,T2}
				,
				Echo["La lista vars deve contenere solo variabili simboliche"];],
			Echo["d dev'essere un vettore reale formato da 3 componenti"];],
		Echo["La massa m dev'essere un numero reale positivo"];
	]
]

(*Questa procedura prende in ingresso la matrice di rotazione R, la matrice delle inerzie II(diagonale), la massa m del corpo, il vettore 
di traslazione d e la lista di variabili vars da cui dipende d. Essa restituisce la matrice delle inerzie generalizzate B e l'energia 
cinetica totale T*) 
energiaCinetica[R_,II_,m_,d_,vars_]:=Module[{sol,B\[Omega],T1,Bt,T2,B,T,s},
	(*Calcolo energie cinetiche*)
	sol=energiaCineticaRot[R,II,vars];
	If[sol===Null,Abort[]];
	{B\[Omega],T1}=sol;

	sol=energiaCineticaTrasl[m,d,vars];
	If[sol===Null,Abort[]];
	{Bt,T2}=sol;

	B=B\[Omega]+Bt//FullSimplify;
	T=T1+T2//FullSimplify;

	(*Sostituzioni per le stampe*)
	s = {
	(* caso singola variabile *)
	Cos[v_] /; MemberQ[vars, v] :> Subscript["c", First @ FirstPosition[vars, v]],
	Sin[v_] /; MemberQ[vars, v] :> Subscript["s", First @ FirstPosition[vars, v]],

	(* caso somma di due variabili *)
	Cos[a_ + b_] /; MemberQ[vars, a] && MemberQ[vars, b] :> 
		With[
			{ia = First @ FirstPosition[vars, a],
			ib = First @ FirstPosition[vars, b]},
			Subscript["c", ia, ib]
		],
	Sin[a_ + b_] /; MemberQ[vars, a] && MemberQ[vars, b] :> 
		With[
			{ia = First @ FirstPosition[vars, a],
			ib = First @ FirstPosition[vars, b]},
			Subscript["s", ia, ib]
			]
	};
	
	Echo[B\[Omega]/.s//MatrixForm,"\!\(\*SubscriptBox[\(B\), \(\[Omega]\)]\)"];
	Echo[Bt/.s//MatrixForm,"\!\(\*SubscriptBox[\(B\), \(t\)]\)"];
	Echo[B/.s//MatrixForm,"B"];
	
	Echo[T1 /.s,"\!\(\*SubscriptBox[\(T\), \(1\)]\)"];
	Echo[T2/.s,"\!\(\*SubscriptBox[\(T\), \(2\)]\)"];
	Echo[T/.s,"T"];

	{B,T}
]

(*Questa procedura prende in ingresso l'altezza h della massa m e restituisce l'energia potenziale gravitazionale della stessa*)
energiaPotenziale[m_,h_]:=If[numeroReale[h]&&numeroReale[m]&&TrueQ[Refine[m>=0]],
	9.81*m*h,
	Echo["Argomenti non validi"];
]

(*Questa procedura prende in ingresso la tabella di Denavit-Hartenberg e restituisce una lista contenente delle matrici di cambiamento delle coordinate per raggiungere il baricentro di ogni link*)
coorBaricentro[tab_]:=Module[{list,Q0i,Qb},
	If[MatrixQ[tab]&&Dimensions[tab][[2]]===7 && Dimensions[tab][[1]]<=6,


		(*Calcolo delle matrici di cambiamento delle coordinate per raggiungere il baricentro di ogni link*)
		list=Table[{},1];
		Q0i=IdentityMatrix[4];
		Do[Q0i=Q0i . cinematicaDiretta[{tab[[i]][[1;;4]]}];
			Qb=Q0i . ArrayFlatten[{{IdentityMatrix[3],Transpose[{{tab[[i,5]],tab[[i,6]],tab[[i,7]]}}]},{0,1}}]//FullSimplify;
			If[i===1,
				list={Qb};,
				list=Append[list,Qb];
			];,

			{i,1,Dimensions[tab][[1]]}
		];
		list

		,
		Echo["Argomento non valido"];
	]
]

(*Questa procedura prende in ingresso la matrice di Denavit-Hartenberg aumentata e la variabili presenti all'interno della tabella di Denavit-Hartenberg. Essa stampa l'energia cinetica e l'energia potenziale di tutti i link e restituisce l'energia cinetica T e l'energia potenziale U totale del robot*)
energiaRobot[tab_,vars_]:=Module[{n,list,R,II,d,T,U,Ti,Ui,s},
	If[MatrixQ[tab]&&Dimensions[tab][[2]]===7 && (n=Dimensions[tab][[1]])<=6,
		If[VectorQ[vars]&&AllTrue[vars,numeroReale[#]&&!NumericQ[#]&],

			(*per la stampa*)
			s={Cos[v_] /; MemberQ[vars, v] :> Subscript["c", First @ FirstPosition[vars, v]],
				Sin[v_] /; MemberQ[vars, v] :> Subscript["s", First @ FirstPosition[vars, v]]}; 
	
			(*Calcolo le matrici di cambiamento delle coordinate per il baricentro*)
			list=coorBaricentro[tab];
			
			(*Calcolo l'energia cinetica e l'energia potenziale di ogni link*)
			T=0;
			U=0;
			Do[
				Print[Style["Link",20,Red,Italic,Bold],Style[i,20,Red,Italic,Bold]];

				R=list[[i]][[1;;3,1;;3]];
				II = DiagonalMatrix[{Subscript["I", ToString[i] <> "x"],Subscript["I", ToString[i] <> "y"],Subscript["I", ToString[i] <> "z"]}];
				d=list[[i]][[1;;3,4]];
				Ti=Assuming[Subscript["m", i]>0,energiaCinetica[R,II,Subscript["m", i],d,vars][[2]]];
				T=T+Ti//FullSimplify;

				Ui=Assuming[Subscript["m", i]>0,energiaPotenziale[Subscript["m", i],d[[3]]]];
				Echo[Ui/.s,"U"];
				U=U+Ui//FullSimplify;
				,
				{i,1,n}
			];

			Print[Style["Risultati",20,Red,Italic,Bold]];
			Echo[T/.s,"Energia cinetica totale"];
			Echo[U/.s,"Energia potenziale gravitazionale totale"];
			{T,U}
			,
			
			Echo["La lista vars deve contenere solo variabili simboliche"];],
		Echo["Il primo argomento non \[EGrave] una tabella di Denavit-Hartenberg aumentata"];
	]
]

(*Questa procedura prende in ingresso l'energia cinetica T, l'energia potenziale U, la funzione di Rayleigh F, il vettore degli ingressi u e le variabili q da cui dipendono T,U e F per poi restituire l'equazione di Eulero-Lagrange associata al sistema. 
NB: essa non controlla che T, U e F dipendano da vars*)
euleroLagrange[T_,U_,F_,u_,vars_]:=Module[{n,L,t,varsT,varsD,varsDT,varsDD,varsDDT,Lt,Ft,LDq,LDt,FDq,res,eq},
	If[VectorQ[u]&&vettoreReale[u,n=Dimensions[u][[1]]],
		If[VectorQ[vars]&&AllTrue[vars,numeroReale[#]&&!NumericQ[#]&] &&Dimensions[u]===Dimensions[vars],

			L=T-U;

			varsT=((#[t])&/@vars);
			varsD=(#')&/@vars;
			varsDT=((#[t])&/@varsD);
			varsDD=(#'')&/@vars;
			varsDDT=((#[t])&/@varsDD);

			Lt=L/. Join[Thread[vars->varsT],Thread[varsD->varsDT]];
			Ft=F/.Thread[varsD->varsDT];

			(*Calcolo \[Delta]L/\[Delta]q*)
			LDq=D[Lt,{varsT}]//FullSimplify;
			LDq=LDq/.Join[Thread[varsT->vars],Thread[varsDT->varsD]];

			(*Calcolo d/dt\[Delta]L/\[Delta]q^\[Prime]*)
			LDt=D[Lt,{varsDT}];
			LDt=D[LDt, t]//FullSimplify;
			LDt=LDt/.Join[Thread[varsT->vars],Thread[varsDT->varsD],Thread[varsDDT->varsDD]];

			(*Calcolo \[Delta]F/\[Delta]q'*)
			FDq=D[Ft,{varsDT}]//FullSimplify;
			FDq=FDq/.Thread[varsDT->varsD];

			(*E.L: d/dt\[Delta]L/\[Delta]q'-\[Delta]L/\[Delta]q+\[Delta]F/\[Delta]q'=u^T*)
			Do[
				eq=FullSimplify[LDt-LDq+FDq][[i]]==FullSimplify[u[[i]]];
				If[i===1, 
					res={eq};,
					res=Append[res,eq];
				]
				,
				{i,1,n}
			];
			res
			,

			Echo["vars dev'essere un vettore contenente tutte le variabili q e della stessa dimensione di u"];],
		Echo["u dev'essere un vettore reale"];
	]
]
