# item_recommend
### pre-installed
- readxl
- devtools
- RCurl
- d3Network
- igraph
- arules

### 프로젝트 설명
- 반려동물 관련 상품 판매 데이터를 분석하여 상품 추천 서비스 제안
#### 분석방법
- 동시 구매고객을 확인하기 위해 구매고객id와 시간을 결합시켜 함께 구매하는 상품에 대한 연관 분석 후 소셜 네트워크 분석 

---

### 연관분석?
- 어떤 두 아이템 집합이 빈번하게 발생하는가에 대한 일련의 규칙들을 생성하는 알고리즘
  - 아이템? 연관분석의 기본단위
  - 아이템 집합? 하나 이상의 아이템 그룹을 대괄호로 묶은 집합    
  ex) A를 구매하는 사람들은 B를 구매할 가능성이 높다는 것에 대한 규칙 생성
- 연관 규칙은 아이템집합의 부분집합으로, 좌측에 있는 아이템 집합(LHS)이 만족되어야 우측에 있는 아이템 집합(RHS)의 결과가 기대됨.
  - LHS에 속해있는 아이템은 RHS에 속할 수 없음. (상호 배반) 
  - LHS가 구매되면 RHS가 구매된다는 뜻.
- 예측 알고리즘이 아닌 패턴을 발견하는 알고리즘(학습이 필요없음)

  
 #### 사용예시
  - 슈퍼마켓에서 함께 판매되는 상품 분석
  - 빈번히 발생하는 DNA패턴과 단백질 서열의 검색
  - 사기성 신용카드 및 보험의 이용과 결합되어 발생하는 의료비 청구의 패턴 발견
  - 휴대폰 서비스 중단에 선행되는 행동의 조합 식별

#### 좋은 규칙 판단 기준?
  1. 지지도(support)
    - 데이터에 발생하는 빈도(빈발 아이템 집합)
    - support(X)=count(X)/N   
      N: 데이터베이스 거래 건수   
      x: 아이템 집합 x를 포함하는 거래 건수   
      
  2. 신뢰도(confidence)
    - 예측 능력이나 정확도의 측정치(아이템 집합 간의 연관성 강도)
    - confidence(X->Y)=support(X,Y)/support(X)
    - **confidence(X->Y)!=confidence(Y->X)**
    
  3. 향상도(lift)
    - 두 개가 동시에 발생할 때 우연일 확률
    - 향상도=1이라면, LHS와 RHS는 서로 독립으로 유의미한 연관성이 없다
    - 향상도=2,두 사건이 독립이라는 가정 대비 2베로 긍정적인 관계
    - 큰 향상도 값은 규칙이 중요하고 아이템 간에 실제 연관성을 반영하는 강한 지표
    - lift(X->Y) = confidence(X->Y)/support(Y)
    -**lift(X->Y)==lift(Y->X)**
    
    **즉, 지지도, 신뢰도, 향상도 모두 높을 경우 효과적인 규칙**
  ---  
 #### Apriori 알고리즘?
  - 아이템 수가 증가할 수록 계산에 소요되는 시간이 기하 급수적으로 증가   
    -> 이를 위해 빈발 집합만을 고려하는 알고리즘
  - Apriori속성: 빈번한 아이템 집합의 모든 부분집합도 빈번해야만 한다.
    ##### 장점
    - 대규모 거래 데이터 작업 가능 
    - 이해하기 쉬운 규칙 생성
    - 예상치 못한 지식 발굴 
    ##### 단점
    - 작은 데이터셋에서는 그다지 유용하지 못함.
    - 진정한 통찰과 상식을 분리하기 위한 노력 필요
    - 랜덤 패턴에서 비논리적인 결론 도출하기 쉬움.
    ##### 방법
    1. 희소 행렬 생성
     - 행: 거래 내역
     - 열: 어떤 사람의 쇼핑백에 나타날 가능성이 있는 모든 아이템   
    ex) 데이터에 100개 종류의 아이템이 있다면 희소 행렬의 열의 수는 100개   
    
    **? 희소 행렬을 사용하는 이유?**
     - 대등한 크기의 행렬이나 데이터 프레임은 거래가 추가되거나 아이템이 더해지면 가용 메모리에 맞추기 어려우나 희소 행렬은 0인 값은 저장하지 않아 메모리를 효율적으로 사용할 수 있다.   
    (희소 행렬은 0이 아닌 값이 매우 적은 행렬로 모든 0의 값을 저장하는 것은 좋지 않아 실제 메모리에 전체 행렬을 저장하지 않음)
    
    2. 데이터 탐색:아이템 빈도(지지도) 그래프 시각화
    3. 알고리즘 적용
    - aprior()함수 내 파라미터 조정
      - 너무 높게 하면 규칙을 찾지 못하거나 너무 포괄적이어서 유용하지 않은 규칙을 찾게됨
      - 너무 낮게 하면 규칙이 너무 많아져서 너무 오래 실행하거나 메모리 부족해질 수 있음.
      - 효과적으로 파라미터를 조정하려면?
        - 지지도: 필요한 최소 거래 건수에 대해 생각하기   
        ex) support=0.1이라면, 아이템이 최소한 (0.1X거래건수) 만큼 거래에 나타나야함.
        - 신뢰도: 신뢰도가 낮으면 엄천안 수의 신뢰할 수 없는 규칙이 생성되고 높으면 뻔하게 예상할 수 있는 규칙이 생성되기 때문에 높은 값에서 낮게 줄이며 조정하는 것이 필요
     4. 평가
     - summary함수를 이용하여 지지도, 신뢰도, 향상도 확인 
        
    
