(ns i18n.pl)

(def translations
  {:missing ":pl missing text"
   :side {:corp "Korpo"
          :runner "Runner"
          :any-side "Dowolna"
          :all "Wszystkie"}
   :faction {:all "Wszystkie"
             :any-faction "Dowolna frakcja"
             :anarch "Anarchowie"
             :criminal "Kryminale"
             :shaper "Kształcerze"
             :adam "Adam"
             :apex "Apex"
             :sunny-lebeau "Sunny Lebeau"
             :jinteki "Jinteki"
             :haas-bioroid "Haas-Bioroid"
             :nbn "NBN"
             :weyland-consortium "Weyland Consortium"
             :neutral "Neutralne"}
   :format {:all "All"
            :any-format "Dowolny format"
            :standard "Standard"
            :eternal "Eternal"
            :core-experience "Core Experience"
            :snapshot "Snapshot"
            :snapshot-plus "Snapshot Plus"
            :socr "SOCR"
            :sunset "Sunset"
            :neo "Neo"
            :classic "Classic"
            :casual "Niekompetytywny"
            :system-gateway "Wejście do Systemu"
            :startup "Startup"}
   :card-type {:all "Wszystkie"
               :identity "Tożsamość"
               :agenda "Projekt"
               :asset "Aktywa"
               :upgrade "Ulepszenie"
               :operation "Operacja"
               :ice "Lód"
               :event "Wydarzenie"
               :hardware "Sprzęt"
               :resource "Zasób"
               :program "Program"}
   :pronouns {:none "nieokreślone"
              :any "dowolne"
              :myodb "bez szczegółów"
              :blank "[puste]"
              :they "oni/ich"
              :she "ona/jej"
              :shethey "ona/ich"
              :he "on/jego"
              :hethey "on/ich"
              :it "ono"
              :ne "ne/nem"
              :ve "ve/ver"
              :ey "ey/em"
              :zehir "ze/hir"
              :zezir "ze/zir"
              :xe "xe/xem"}
   :chat {:title "Graj w Netrunnera w przeglądarce"
          :channels "Kanały"
          :send "Wyślij"
          :placeholder "Powiedz coś..."
          :delete "Usuń wiadomość"
          :delete-all "Usuń wszystkie wiadomości użytkownika"
          :block "Zablokuj użytkownika"
          :cancel "Anuluj"}
   :nav {:chat "Czat"
         :cards "Karty"
         :deck-builder "Tworzenie talii"
         :play "Graj"
         :help "Pomoc"
         :settings "Ustawienia"
         :stats "Statystyki"
         :about "Informacje"
         :tournaments "Turnieje"
         :admin "Administracja"
         :users "Użytkownicy"
         :features "Cechy"
         :game-count (fn [[cnt]] (str "Gry: " cnt))}
   :menu {:settings :pl.nav/settings
          :logout "Wylogowanie"
          :admin :pl.nav/admin
          :moderator "Moderator"}
   :card-browser {:search-hint "Szukaj kart"
                  :sort "Porządkowanie"
                  :format "Format"
                  :set "Zestaw"
                  :side "Strona"
                  :faction "Frakcja"
                  :type "Typ"
                  :clear "Wyczyść"
                  :select-art "Wybierz grafiki"
                  :selected-art "Wybrano grafiki alt."
                  :update-success "Grafiki zaktualizowane"
                  :update-failure "Nieudana aktualizacja grafiki"
                  :memory "Pamięć"
                  :cost "Koszt"
                  :trash-cost "Koszt skasowania"
                  :strength "Siła"
                  :advancement "Wymagany poziom rozwoju"
                  :agenda-points "Punkty projektów"
                  :min-deck "Minimalny rozmiar talii"
                  :inf-limit "Limit wpływu"
                  :influence "Wpływ"
                  :sort-by {:faction "Frakcja"
                            :name "Nazwa"
                            :type "Typ"
                            :influence "Wpływ"
                            :cost "Koszt"
                            :set-number "Numer w zestawie"}}
   :deck-builder {:loading-msg "Ładowanie kolekcji talii..."
                  :new-corp "Nowa talia Korpo"
                  :new-runner "Nowa talia Runnera"
                  :import-button "Zaimportuj talię"
                  :reset "Zresetuj"
                  :import-title "Wpisz ID lub URL talii z NRDB (musi być opublikowana)"
                  :import "Importuj"
                  :cancel "Anuluj"
                  :import-placeholder "ID z NRDB"
                  :deck-count (fn [[cnt]] (str "Talie: " cnt))
                  :filtered "(filtrowane)"
                  :save "Zapisz"
                  :confirm-delete "Potwierdź usunięcie"
                  :edit "Edytuj"
                  :delete "Usuń"
                  :clear-stats "Usuń statystyki"
                  :create-game "Stwórz stół"
                  :deck-name "Nazwa talii"
                  :format "Format"
                  :identity "Tożsamość"
                  :deck-notes "Notatki"
                  :decklist "Lista kart"
                  :decklist-inst "(Wpisz lub wklej listę kart z talii, zostanie przeanalizowana)"
                  :notes "Notatki"
                  :add-to-deck "Dodaj do talii"
                  :add-cards "Dodaj karty"
                  :card-name "Nazwa karty"
                  :no-decks "Brak talii"
                  :cards "karty"
                  :min "min."
                  :max "maks."
                  :influence "Wpływ"
                  :agenda-points "Punkty projektów"
                  :deck-points "Punkty talii"
                  :hash "Kod turniejowy"
                  :why "Dlaczego?"
                  :legal "legalny"
                  :illegal "nielegalny"
                  :games "Gry"
                  :completed "Ukończone"
                  :won "Wygrane"
                  :lost "Przegrane"}
   :lobby {:no-games "Brak gier"
           :tournament "Wydarzenia"
           :competitive "Kompetytywne"
           :casual "Niekompetytywne"
           :new-game "Nowa gra"
           :reload "Odśwież listę"
           :load-replay "Wczytaj powtórkę"
           :start-replay "Rozpocznij powtórkę"
           :save-replay "Zapisz powtórkę"
           :replay-link-error "Nieprawidłowy URL powtórki."
           :replay-invalid-file "Wybierz prawidłowy plik powtórki."
           :create "Stwórz"
           :cancel "Anuluj"
           :title "Tytuł"
           :side "Strona"
           :format "Format"
           :options "Opcje"
           :spectators "Dozwoleni widzowie"
           :hidden "Ukryte informacje graczy widoczne dla widzów"
           :password-protected "Ochrona hasłem"
           :password "Hasło"
           :start "Rozpocznij"
           :leave "Wyjdź"
           :swap "Zmień stronę"
           :waiting "Oczekiwanie na wybór talii"
           :players "Gracze"
           :deck-selected "Talia wybrana"
           :select-deck "Wybierz talię"
           :chat "Czat"
           :select-title "Wybierz talię"
           :spectator-count (fn [[cnt]] (str "Widownia: " cnt))
           :closed-msg "Poczekalnia zamknięta z powodu braku aktywności"
           :title-error "Wpisz nazwę dla stołu."
           :password-error "Wpisz hasło."
           :too-little-data "Zbyt mało danych"
           :completion-rate "Procent zakończonych gier"
           :watch "Oglądaj"
           :join "Dołącz"
           :rejoin "Wróć"
           :as-corp "jako Korpo"
           :as-runner "jako Runner"
           :private "PRYWATNY"
           :reset "Zresetuj nazwę stołu"
           :delete "Usuń stół"
           :password-for "Hasło dla"
           :invalid-password "Nieprawidłowe hasło"
           :not-allowed "Niedozwolone"
           :aborted "Połączenie przerwane"
           :api-access "Zezwól na dostęp API do danych rozgrywki"
           :api-requires-key "(Wymaga podania klucza API w ustawieniach)"
           :game-count (fn [[cnt]] (str "Gry: " cnt))
           :filtered "(filtrowane)"}
   :settings {:invalid-password "Nieprawidłowa nazwa lub hasło"
              :invalid-email "Brak konta z tym adresem e-mail"
              :updated "Profil zaktualizowany - odśwież okno przeglądarki"
              :updating "Aktualizacja profilu..."
              :get-log-width "Pobierz aktualną szerokość dziennika"
              :get-log-top "Pobierz aktualną wysokość dziennika"
              :email-title "Zmień adres e-mail"
              :current-email "Aktualny e-mail"
              :desired-email "Nowy e-mail"
              :email-placeholder "Adres e-mail"
              :enter-valid "Podaj poprawny adres e-mail"
              :update "Aktualizuj"
              :cancel "Anuluj"
              :email "E-mail"
              :change-email "Zmień adres e-mail"
              :avatar "Awatar"
              :change-avatar "Zmień na stronie gravatar.com"
              :pronouns "Zaimki osobowe"
              :language "Język"
              :sounds "Dźwięki"
              :enable-lobby-sounds "Włącz dźwięki poczekalni"
              :enable-game-sounds "Włącz dźwięki gry"
              :volume "Głośność"
              :layout-options "Opcje układu stołu"
              :stacked-cards "Grupowanie kart (domyślnie włączone)"
              :runner-layout "Układ stołu Runnera ze strony Korpo"
              :runner-classic "Karty Runnera w klasycznym stylu (od góry: Programy, Sprzęt, Zasoby)"
              :runner-reverse "Karty Runnera w odwróconym stylu (od góry: Zasoby, Sprzęt, Programy)"
              :background "Tło stołu"
              :card-backs "Rewersy kart"
              :game-stats "Statystyki wygranych/przegranych"
              :deck-stats "Statystyki talii"
              :always "Zawsze"
              :comp-only "Tylko w grach kompetytywnych"
              :none "Nigdy"
              :alt-art "Alternatywne grafiki"
              :show-alt "Pokaż alternatywne grafiki kart"
              :high-res "Użyj obrazów kart o wysokiej rozdzielczości"
              :card-images "Obrazy kart"
              :set-all "Ustaw wszystkie karty na"
              :set "Ustaw"
              :reset "Zresetuj wszystko do oficjalnych grafik"
              :blocked "Zablokowani użytkownicy"
              :user-name "Nazwa użytkownika"
              :block "Zablokuj użytkownika"
              :update-profile "Aktualizuj profil"
              :nsg "NSG"
              :ffg "FFG"
              :api-keys "Klucze API"
              :delete-api-key "Usuń"
              :create-api-key "Stwórz klucz API"}
   :stats {:game-stats "Statystyki gier"
           :corp-stats "Statystyki: Korpo"
           :runner-stats "Statystyki: Runner"
           :clear-stats "Wyczyść statystyki"
           :no-log "Brak dziennika"
           :view-log "Obejrzyj dziennik"
           :winner "Wygrana"
           :no-games "Brak gier"
           :all-games "Pokaż wszystkie gry"
           :shared-games "Pokaż jedynie udostępnione"
           :started "Rozpoczęteo"
           :ended "Zakończono"
           :completed "Ukończono"
           :not-completed "Nie ukończono"
           :won "Wygrano"
           :lost "Przegrano"
           :turn-count (fn [[cnt]] (str "Tura: " cnt))
           :lobby "Poczekalnia"
           :format "Format"
           :win-method "Sposób wygranej"
           :view-games "Powrót do ekranu statystyk"
           :share "Udostępnij powtórkę"
           :launch "Uruchom powtórkę"
           :download "Pobierz powtórkę"
           :unavailable "Powtórka niemożliwa"
           :filtered "(filtrowane)"
           :log-count (fn [[cnt]] (str "Dzienniki: " cnt))
           :clicks-gained "Zyskane kliknięcia"
           :credits-gained "Zyskane kredyty"
           :credits-spent "Wydane kredyty"
           :credits-click "Kredyty zyskane przez kliknięcia"
           :cards-drawn "Dobrane karty"
           :cards-click "Karty dobrane przez kliknięcia"
           :damage-done "Zadane obrażenia"
           :cards-rezzed "Aktywowane karty"
           :tags-gained "Otrzymane znaczniki namiaru"
           :runs-made "Rozpoczęte włamów"
           :cards-accessed "Dostępy do kart"}
   :log {:game-log "Dziennik gry"
         :annotating "Notatki"
         :shared "Udostępnione notatki"
         :remote-annotations-fail "Nieudane pobranie zdalnych notatek."}
   :annotations {:turn-placeholder "Notatki do tej tury"
                 :click-placeholder "Notatki do tego kliknięcia"
                 :available-annotations "Dostępne notatki"
                 :no-published-annotations "Brak opublikowanych notatek."
                 :publish "Publikuj"
                 :import-local "Zaimportuj plik z notatkami"
                 :load-local "Wczytaj"
                 :save-local "Zapisz"
                 :clear "Wyczyść lokalne notatki"}
   :game {:keep "Zachowanie"
          :mulligan "Wymiana"
          :close "Zamknięcie"
          :start "Rozpoczęcie gry"
          :remove-tag "Usunięcie 1 namiaru"
          :run "Rozpoczęcie włamu"
          :purge "Usunięcie wirusów"
          :trash-resource "Skasowanie 1 zasobu"
          :draw "Dobranie karty"
          :gain-credit "Zyskanie 1 kredytu"
          :game-start "Rozpoczęcie gry"
          :start-turn "Rozpoczęcie tury"
          :end-turn "Zakończenie tury"
          :mandatory-draw "Przymusowe dobranie"
          :take-clicks "Rozpoczęcie kliknięć"
          :hq "KG"
          :grip "Garść"
          :rfg "Usunięte z gry"
          :play-area "Zagrywane"
          :current "Wątek"
          :scored-area "Punktacja"
          :archives "Archiwum"
          :max-hand "Maks. ilość kart"
          :brain-damage "Obrażenia mózgu"
          :tag-count (fn [[base additional total]] (str "Namiar :" base (when (pos? additional) (str " + " additional))))
          :agenda-count (fn [[agenda-point]] (str "Punkty projektów: " agenda-point))
          :link-strength "Łącze"
          :credit-count (fn [[credit run-credit]] (str "Kredyty: " credit (when (pos? run-credit) (str " (" run-credit " na włam)"))))
          :click-count (fn [[click]] (str "Kliki: " click))
          :bad-pub-count (fn [[base additional]] (str "Zła prasa: " base (when (pos? additional) (str " + " additional))))
          :mu-count (fn [[unused available]] (str "Wolne: " unused " z " available " JP"))
          :special-mu-count (fn [[unused available mu-type]] (str "Wolne: " unused " z " available " " mu-type " JP"))
          :indicate-action "Zgłoszenie działania"
          :spec-count (fn [[c]] (str "Widownia: " c))
          :spec-view "Perspektywa widowni"
          :runner-view "Perspektywa Runnera"
          :corp-view "Perspektyw Korpo"
          :leave-replay "Wyjście z powtórki"
          :leave "Wyjście z gry"
          :unmute "Włączenie czat widowni"
          :mute "Wyłączenie czat widowni"
          :concede "Poddanie się"
          :inactivity "Gra zakończona z powodu braku aktywności"
          :server "Serwer"
          :unimplemented "Niezautomatyzowana"
          :abilities "Opcje"
          :let-subs-fire "Pozwolenie na użycie niezłamanych poleceń"
          :subs "Polecenia"
          :actions "Akcje"
          :fire-unbroken "Użycie niezłamanych poleceń"
          :stack "Stos"
          :r&d "DBR"
          :shuffle "Przetasowanie"
          :show "Pokazanie"
          :close-shuffle "Zamknięcie i przetasowanie"
          :heap "Sterta"
          :card-count (fn [[size]] (str "Karty: " size "."))
          :face-down-count (fn [[total face-up]] (str "Karty: " total ", awersem w dół:" (- total face-up) "."))
          :up-down-count (fn [[total face-up]] (str face-up "↑ " (- total face-up) "↓"))
          :initiation "Rozpoczęcie"
          :approach-ice "Zbliżenie do lodu"
          :encounter-ice "Napotkanie lodu"
          :movement "Ruch"
          :breach-server "Przebicie do serwera"
          :success "Sukces"
          :run-ends "Włam się kończy"
          :no-current-run "Brak włamu w toku"
          :current-phase "Aktualna faza"
          :unknown-phase "Nieznana faza"
          :rez "Aktywacja"
          :no-further "Brak dalszych działań"
          :continue "Kontynuacja"
          :continue-to "Kontynuacja do:"
          :stop-auto-pass "Wyłączenie priorytetu auto-mijania"
          :auto-pass "Priorytet auto-mijania"
          :jack-out "Wylogowanie się"
          :trace "Namiar"
          :credits "kred."
          :card "Karta"
          :time-taken (fn [[t]] (str "Czas gry: " t " min."))
          :win-decked (fn [[turn]] (str "wygrywa, ponieważ Korpo skończyły się karty w " turn " turze."))
          :win-flatlined (fn [[turn]] (str "wygrywa przez wypłaszczenie w " turn " turze."))
          :win-conceded (fn [[turn]] (str "wygrywa przez poddanie się przeciwnika w " turn " turze."))
          :win-claimed (fn [[turn]] (str "wygrywa przez zgłoszenie wygranej w " turn " turze."))
          :win-points (fn [[turn]] (str "wygrywa przez zdobycie punktów zwycięstwa w " turn " turze."))
          :win-other (fn [[turn reason]] (str "wygrywa przez " reason " w " turn " turze."))}})
