# Лабораторная работа 2

## Вариант `sc-set`

---

  * Студент: `Бушмелев Константин Алексеевич`
  * Группа: `P3318`
  * ИСУ: `367950`

---

## Требования

Интерфейс — `Set`, структура данных — `Separate Chaining HashMap`.

1. Функции:
    * [x] добавление и удаление элементов;
    * [x] фильтрация;
    * [x] отображение (`map`);
    * [x] свертки (левая и правая);
    * [x] структура должна быть [моноидом](https://ru.m.wikipedia.org/wiki/%D0%9C%D0%BE%D0%BD%D0%BE%D0%B8%D0%B4).
2. Структуры данных должны быть **неизменяемыми**.
3. Библиотека должна быть протестирована в рамках **unit testing**.
4. Библиотека должна быть протестирована в рамках **property-based** тестирования (*как минимум 3 свойства*, включая свойства моноида).
5. Структура должна быть **полиморфной**.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка — необходимо реализовать их вручную и по возможности — обеспечить совместимость.

---

## Ключевые элементы реализации

Структура данных

```haskell
data SCSet v = SCSet {
    currentSize :: Int,
    maxSize :: Int,
    loadFactor :: Float,
    arrayData :: Array Int [v]
}
```

Добавление, получение и удаление элементов:

```haskell
createSet :: SCSet v
createSet =
    let initialMaxSize = 8
        initialLoadFactor = 0.75
        initialCurrentSize = 0
    in SCSet
    { currentSize = initialCurrentSize
    , maxSize = initialMaxSize
    , loadFactor = initialLoadFactor
    , arrayData = array (0, initialMaxSize - 1) [(i, []) | i <- [0..initialMaxSize - 1]]
    }

insertAtBeginning :: (Eq v) => Array Int [v] -> Int -> v -> Array Int [v]
insertAtBeginning arr pos value = arr // [(pos, value : filter (/= value) (arr ! pos))]

getDataAsList :: SCSet v -> [v]
getDataAsList scset = concat (elems (arrayData scset))

addAll :: (Hashable v) => [v] -> SCSet v -> SCSet v
addAll xs scset = foldr add scset xs

add :: (Hashable v) => v -> SCSet v -> SCSet v
add value scset =
    if fromIntegral (currentSize scset + 1) >= loadFactor scset * fromIntegral (maxSize scset)
    then redefineSet
    else insertNormal
    where
        redefineSet =
            add value (addAll listOfElem generatedSet)
            where
                generatedSet = SCSet {
                    maxSize = maxSize scset * 2,
                    currentSize = 0,
                    loadFactor = 0.75,
                    arrayData = array (0, maxSize scset * 2 - 1) [(i, []) | i <- [0..maxSize scset * 2 - 1]]
                }
                listOfElem = getDataAsList scset
        insertNormal =
            SCSet {
                currentSize = currentSize scset + (if null (arrayData scset ! position) then 1 else 0),
                maxSize = maxSize scset,
                loadFactor = loadFactor scset,
                arrayData = insertAtBeginning (arrayData scset) position value
            }
            where
                position = hash value `mod` maxSize scset

removeValue :: (Eq v) => v -> [v] -> [v]
removeValue _ [] = []
removeValue value arr = filter (/= value) arr

remove :: (Hashable v) => v -> SCSet v -> SCSet v
remove value scset =
    SCSet newCurrentSize (maxSize scset) (loadFactor scset) newData
    where
        position = hash value `mod` maxSize scset
        oldData = arrayData scset
        newData = if null (oldData ! position)
            then oldData
            else oldData // [(position, removeValue value (oldData ! position))]
        newCurrentSize = currentSize scset - (if null (oldData ! position) then 0 else 1)
```

Фильтрация:

```haskell
filterValues :: (Hashable v) => (v -> Bool) -> SCSet v -> SCSet v
filterValues predicate scset =
    let valuesList = concat (elems (arrayData scset))
        filteredValues = filter predicate valuesList
    in addAll filteredValues createSet 
```

Отображение (`map`):

```haskell
mapValues :: (Hashable v) => (v -> v) -> SCSet v -> SCSet v
mapValues f scset =
    let valuesList = concat (elems (arrayData scset))
        newValues = map f valuesList
    in addAll newValues createSet
```

Свертки (левая и правая):

```haskell
instance Foldable SCSet where
    foldMap f scset = mconcat (map f (getDataAsList scset))
```

### Соответствие свойству [моноида](https://ru.m.wikipedia.org/wiki/%D0%9C%D0%BE%D0%BD%D0%BE%D0%B8%D0%B4)

```haskell
instance (Hashable v, Eq v) => Semigroup (SCSet v) where
    (SCSet size1 maxSize1 loadFactor1 arr1) <> (SCSet size2 maxSize2 loadFactor2 arr2) =
        let newArr = foldr add (SCSet 0 (max maxSize1 maxSize2) loadFactor1 (array (0, max maxSize1 maxSize2 - 1) [(i, []) | i <- [0..max maxSize1 maxSize2 - 1]])) (getDataAsList (SCSet size1 maxSize1 loadFactor1 arr1))
            in foldr add newArr (getDataAsList (SCSet size2 maxSize2 loadFactor2 arr2))

instance (Hashable v, Eq v) => Monoid (SCSet v) where
    mempty = createSet
    mappend = (<>)
```

## Тестирование

В рамках данной работы были применены два инструмента:

  * HUnit - для модульного тестирования;
  * QuickCheck - для тестирования свойств (property-based).
