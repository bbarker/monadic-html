package mhtml.todo

import scala.scalajs.js.JSApp
import scala.xml.Node
import scala.collection.breakOut
import cats.implicits._
import cats.kernel.Semigroup
import mhtml._
import mhtml.implicits.cats._
import org.scalajs.dom
import org.scalajs.dom.{Event, KeyboardEvent}
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.ext.LocalStorage
import org.scalajs.dom.raw.HTMLInputElement
import upickle.default.read
import upickle.default.write

sealed abstract class AbstractComponent[D](view: Node, model: Rx[D])
case class Component[D](view: Node, model: Rx[D]) extends AbstractComponent[D](view, model)

/**
  * TaggedComponent is useful for updating a collection of components where one would want to
  * sometimes alter Component[D] based on input data of type T
  */
case class TaggedComponent[D,T](view: Node, model: Rx[D], tag: T) extends AbstractComponent[D](view, model)

/**
  * Alternative to using TaggedComponent; tags are replaces by keys in a map. This is not a true
  * list of components, but rahter a component that simulates a list of components.
  * @param view
  * @param model
  * @param tag
  * @tparam D
  * @tparam T
  */
case class ComponentList[D,T](
  view: Node,
  store: Rx[Map[T,D]],
  orderMaybe: Option[Ordering[T]]
)
  extends AbstractComponent[List[D]](
    view,
    store.map(storeNow =>
      orderMaybe match {
        case Some(order) =>
          storeNow.keys.toList.sorted(order).map(key => storeNow(key))
        case None => storeNow.values.toList
      }
    )
  )

case class Todo(title: String, completed: Boolean)

case class TodoList(text: String, hash: String, items: Rx[List[Todo]])

sealed trait TodoEvent
final case class UpdateEvent(oldTodo: Todo, newTodo: Todo) extends TodoEvent
final case class AddEvent(newTodo: Todo) extends TodoEvent
final case class RemovalEvent(todo: Todo) extends TodoEvent


object MhtmlTodo extends JSApp {
  type Store = Map[Todo, Option[TodoEvent]]
  val allTodosProxy: Var[List[Todo]] = Var(Nil)

  val all = TodoList("All", "#/", allTodosProxy.dropRepeats)
  val active = TodoList("Active", "#/active", allTodosProxy.map(_.filter(!_.completed)).dropRepeats)
  val completed = TodoList("Completed", "#/completed", allTodosProxy.map(_.filter(_.completed)).dropRepeats)
  val todoLists = List(all, active, completed)

  val windowHash: Rx[String] = Rx(dom.window.location.hash).merge {
    val updatedHash = Var(dom.window.location.hash)
    dom.window.onhashchange = (ev: Event) => {
      updatedHash := dom.window.location.hash
    }
    updatedHash
  }.dropRepeats

  val currentTodoList: Rx[TodoList] = windowHash.map { hash =>
    todoLists.find(_.hash === hash).getOrElse(all)
  }.dropRepeats

  //type TLCmp = TaggedComponent[Option[TodoEvent], Todo]

  val todoListEventProxy: Var[Option[TodoEvent]] = Var(None)
  //TODO: this is merge: consider merge on TODOs to go from N^2 to N ... otherwise this gets really slow
  val todoListComponents: ComponentList[Option[TodoEvent], Todo] = {

    val todoListTodos: Rx[List[Todo]] = currentTodoList.map(tl => tl.items).flatten.dropRepeats
    type TLCMonitors = (TodoList, List[Todo], Option[TodoEvent])
    val ctlTodosZipped: Rx[TLCMonitors] =
      for (ctl <- currentTodoList; tlt <- todoListTodos; tle <- todoListEventProxy)
        yield (ctl, tlt, tle)

    val store: Rx[Store] = ctlTodosZipped.foldp(
      Rx(Map[Todo, Option[TodoEvent]]())// Start with an empty collection
    ) {
      (storeNow: Rx[Store], tlcMonitors: TLCMonitors) =>
        //FIXME: do we really need ev???
        val (currentTodoList, currentTodos, ev) = tlcMonitors
        (storeNow |@| currentTodoList.items).map {
          (currentComps: Store, currentTodos: List[Todo]) =>
            currentTodos.map { todo =>
              println(s"currentComps size is ${currentComps.size}")
              currentComps.get(todo) match {
                case Some(todoComp) => todo -> todoComp
                case None => todo -> todoListItem(todo)
              }
            }.toMap
        }
    }.flatten.dropRepeats.map{tlc => println(s"got todoListComponents change (predrop)"); tlc}.map{tlc => println(s"got todoListComponents change"); tlc} // DEBUG

    ComponentList(<div/>, store)
  }

//  val todoListComponents: Rx[List[TLCmp]] = {
//    val todoListTodos: Rx[List[Todo]] = currentTodoList.map(tl => tl.items).flatten.dropRepeats
//    type TLCMonitors = Rx[(TodoList, List[Todo], Option[TodoEvent])]
//    val ctlTodosZipped: TLCMonitors =
//      for (ctl <- currentTodoList; tlt <- todoListTodos; tle <- todoListEventProxy) yield (ctl, tlt, tle) //currentTodoList zip todoListTodos zip todoListEventProxy
//    ctlTodosZipped.foldp(Rx(List[TLCmp]())) {
//      case (tlCmps: Rx[List[TLCmp]], (currentTodoList: TodoList, currentTodos: List[Todo], ev: Option[TodoEvent])) =>
//        println(s"DEBUG: got event $ev in todoListComponents")
//        (tlCmps |@| currentTodoList.items).map {
//          case (currentComps: List[TLCmp], currentTodos: List[Todo]) =>
//            currentTodos.map { todo =>
//              val idx = currentComps.indexWhere { cmp => cmp.tag == todo }
//              println(s"currentComps size is ${currentComps.size}")
//              println(s"comp index is $idx") // DEBUG
//              if (idx >= 0) currentComps(idx)
//              else todoListItem(todo)
//            }
//        }
//    }.flatten.dropRepeats.map{tlc => println(s"got todoListComponents change (predrop)"); tlc}.map{tlc => println(s"got todoListComponents change"); tlc} // DEBUG
//  }

//  val todoListComponents: Rx[List[TLCmp]] =
//  currentTodoList.foldp(Rx(List[TLCmp]())){(tlCmps: Rx[List[TLCmp]], currentTodoList: TodoList) =>
//    (tlCmps |@| currentTodoList.items).map {
//      case (currentComps: List[TLCmp], currentTodos: List[Todo]) =>
//        currentTodos.map{ todo =>
//          val idx = currentComps.indexWhere { cmp => cmp.tag == todo }
//          println(s"currentComps size is ${currentComps.size}")
//          println(s"comp index is $idx") // DEBUG
//          if (idx >= 0) currentComps(idx)
//          else todoListItem(todo)
//        }
//    }
//  }.flatten.dropRepeats


  //DEBUG
  todoListComponents.impure.foreach(tlc => println(s"tlc size = ${tlc.size}"))


  //FIXME: fold with flatmap is potentially problematic: https://github.com/OlivierBlanvillain/monadic-html
  val todoListEvent: Rx[Option[TodoEvent]] = {
    println("in todoListEvent definition") // DEBUG
    val todoListEventDef = todoListComponents.map(compList => compList.map(comp => comp.model))
      .flatMap {todoListModels =>
        println(s"inside flatmap for todoListEventDef: todoListModels size = ${todoListModels.size} ")
        todoListModels.foldRight[Rx[Option[TodoEvent]]](Rx(None))(
          (lastEv: Rx[Option[TodoEvent]], nextEv: Rx[Option[TodoEvent]]) => nextEv |+| lastEv
        )
      }.dropRepeats.map { tle => println(s"got tlEvent $tle from todoListComponents (DEBUG)"); tle } // DEBUG
    todoListEventProxy.imitate(todoListEventDef)
  }

  todoListEvent.impure.foreach(tle => println(s"tle = $tle"))

  val header: Component[Option[AddEvent]] = {
    val newTodo = Var[Option[AddEvent]](None)

    def onInputKeydown(event: KeyboardEvent): Unit = {
      (event.currentTarget, event.keyCode) match {
        case (input: HTMLInputElement, KeyCode.Enter) =>
          input.value.trim match {
            case "" =>
            case title =>
              newTodo := Some(AddEvent(Todo(title, completed = false)))
              input.value = ""
          }
        case _ =>
      }
    }

    val headerNode =
      <header class="header">
        <h1>todos</h1>
        <input class="new-todo"
               autofocus="true"
               placeholder="What needs to be done?"
               onkeydown={onInputKeydown _}/>
      </header>
    Component(headerNode, newTodo.dropRepeats)
  }

  val footer: Component[Option[RemovalEvent]] = {
    val removeTodo = Var[Option[RemovalEvent]](None)
    val display = allTodosProxy.map(x => if (x.isEmpty) "none" else "")
    val visibility =
      completed.items.map(x => if (x.isEmpty) "hidden" else "visible")
    val footerNode =
      <footer class="footer" style:display={display}>
        <ul class="filters">
          {todoLists.map(todoListsFooter)}
        </ul>
        <button onclick={() =>
          println("DEBUG: footer clicked")
          allTodos.map(_.filter(_.completed).foreach(todo =>
            removeTodo := Some(RemovalEvent(todo))
          ))
          ()}
                class="clear-completed"
                style:visibility={visibility}>
          Clear completed
        </button>
      </footer>
    Component(footerNode, removeTodo.dropRepeats)
  }

  val anyEvent: Rx[Option[TodoEvent]] =
    (todoListEvent |+| footer.model |+| header.model).dropRepeats

  anyEvent.impure.foreach(ev => println(s"anyEvent: $ev")) //DEBUG

  val allTodos: Rx[List[Todo]] = anyEvent.foldp(load()) {
    (last, ev) => updateState(last, ev)
  }.dropRepeats
  val allTodosRunner: Rx[Node] = allTodosProxy.imitate(allTodos)
    .dropRepeats.map { _ => {
      <div/>
  }}

  def todoListItem(todo: Todo): Component[Option[TodoEvent]] = {
    println(s"making new todo component for $todo") // DEBUG
    val removeTodo = Var[Option[RemovalEvent]](None)
    val updateTodo = Var[Option[UpdateEvent]](None)
    val suppressOnBlur = Var(false)

    def submit(event: Event) = {
      suppressOnBlur := true
      editingTodo := None
      event.currentTarget.asInstanceOf[HTMLInputElement].value.trim match {
        case "" =>
          removeTodo := Some(RemovalEvent(todo))
        case trimmedTitle =>
          updateTodo := Some(UpdateEvent(todo, Todo(trimmedTitle, todo.completed)))
      }
    }

    def onEditTodoTitle(event: KeyboardEvent): Unit = {
      event.keyCode match {
        case KeyCode.Escape =>
          suppressOnBlur := true
          editingTodo := None
        case KeyCode.Enter =>
          submit(event)
          focusInput()
        case _ =>
      }
    }

    def ignoreEvent(event: Event): Unit = ()

    def blurHandler: Rx[Event => Unit] =
      suppressOnBlur.map(x => if (x) ignoreEvent(_) else submit(_)).dropRepeats

    def onToggleCompleted(event: Event): Unit = {
      println("DEBUG: onToggleCompleted")
      event.currentTarget match {
        case input: HTMLInputElement =>
          updateTodo := Some(
            UpdateEvent(todo, Todo(todo.title, input.checked))
          )
        case _ =>
      }
    }

    def onDoubleClick(event: Event): Unit = {
      editingTodo := Some(todo)
      focusInput()
    }

    val onDelete: (Event) => Unit = _ =>
      removeTodo := Some(RemovalEvent(todo))

    val css = editingTodo.map { x =>
      val editing = if (x.contains(todo)) "editing" else ""
      val completed = if (todo.completed) "completed" else ""
      s"$editing $completed"
    }
    val data: Rx[Option[TodoEvent]] = Semigroup[Rx[Option[TodoEvent]]].combine(removeTodo, updateTodo)
    data.impure.foreach(ev => println(s"DEBUG: todoListItem event: $ev"))
    val todoListElem =
      <li class={css}>
        <div class="view">
          <input onclick={onToggleCompleted _}
                 class="toggle"
                 type="checkbox"
                 checked={todo.completed}/>
          <label ondblclick={onDoubleClick _}>
            {todo.title}
          </label>
          <button onclick={onDelete} class="destroy"></button>
        </div>
        <input onkeydown={onEditTodoTitle _}
               id="editInput"
               class="edit"
               value={todo.title}
               onblur={blurHandler}/>
      </li>
    Component(todoListElem, data.dropRepeats.map{ev => s"${todo.title} got event: ${ev}"; ev})
  }

  val todoListElems: Rx[List[Node]] =
    todoListComponents.map { tlcSeq => tlcSeq.map(comp => comp.view) }.dropRepeats

  val mainSection: Component[List[UpdateEvent]] = {
    val todoUpdates = Var[List[UpdateEvent]](Nil)

    def setAllCompleted(todosIn: Seq[Todo], completed: Boolean): List[UpdateEvent] =
      todosIn.flatMap {
        case todo if todo.completed != completed =>
          Some(UpdateEvent(todo, Todo(todo.title, completed)))
        case _ => None
      }(breakOut)

    // TODO(olafur) This is broken in 0.1, fix here https://github.com/OlivierBlanvillain/monadic-html/pull/9
    val checked = active.items.map(x => x.isEmpty)
    val display = allTodos.map(todos => if (todos.isEmpty) "none" else "")

    def setAllCompletedHandler(event: Event): Unit = {
      event.currentTarget match {
        case input: HTMLInputElement =>
          allTodos.map(todos => setAllCompleted(todos, input.checked))
            .map(todos => todoUpdates := todos)
          ()
        case _ => ()
      }
    }

    val mainDiv =
      <section class="main" style:display={display}>
        <input onclick={setAllCompletedHandler _}
               type="checkbox"
               class="toggle-all"
               checked={checked}/>
        <label for="toggle-all" checked={checked}>Mark all as complete</label>
        <ul class="todo-list">
          {todoListElems}
        </ul>
      </section>
    Component(mainDiv, todoUpdates.dropRepeats)
  }

  //  object Model {
  val LocalStorageName = "todo.mhtml"

  def load(): List[Todo] =
    LocalStorage(LocalStorageName).toSeq.flatMap(read[List[Todo]]).toList

  def save(todos: List[Todo]): Unit =
    LocalStorage(LocalStorageName) = write(todos)

  val editingTodo: Var[Option[Todo]] = Var[Option[Todo]](None)

  def unzipListComponents(listComps: Seq[Component[List[TodoEvent]]])
  : (List[Node], List[Rx[List[TodoEvent]]])
    = listComps.toList.map(tlc => (tlc.view, tlc.model)).unzip

  case class ModelSources(
    headerModel: Option[Todo],
    changes:  List[TodoEvent]
  )

  def updateState(currentTodos: List[Todo], evOpt: Option[TodoEvent]): List[Todo] = {
    println(s"running updateState with event $evOpt on $currentTodos")
    evOpt match {
    case Some(AddEvent(newTodo)) =>
      println("DEBUG: AddEvent(newTodo)")
      newTodo :: currentTodos
    case Some(RemovalEvent(rmTodo)) => currentTodos.filter(todo => todo == rmTodo)
    case Some(UpdateEvent(oldTodo, newTodo)) =>
      val listIndex = currentTodos.indexOf(oldTodo)
      if (listIndex > 0) {
        println(s"DEBUG: newTodo is $newTodo")
        currentTodos.updated(listIndex, newTodo)
      }
      else {
        println("DEBUG: just returning currentTodos")
        currentTodos
      }
    case None => {
      println("DEBUG: just returning currentTodos (None)")
      currentTodos
    }
  }
}

  def focusInput(): Unit =
    dom.document.getElementById("editInput") match {
      case t: HTMLInputElement => t.focus()
      case _ =>
    }

  val count = active.items.map { items =>
    <span class="todo-count">
      <strong>{ items.length }</strong>
      {if (items.length === 1) "item" else "items"} left
    </span>
  }

  def todoListsFooter(todoList: TodoList) = {
    val css = currentTodoList.map(x => if (x == todoList) "selected" else "")
    <li>
      <a href={ todoList.hash } class={ css }>{ todoList.text }</a>
    </li>
  }

  val todoapp: Node = {
    <div>
      { allTodosRunner }
      <section class="todoapp">{ header.view }{ mainSection.view }{ footer.view }</section>
      <footer class="info">
        <p>Double-click to edit a todo</p>
        <p>
          Originally written by <a href="https://github.com/atry">Yang Bo</a>,
          adapted to monadic-html by <a href="https://github.com/olafurpg">Olafur Pall Geirsson</a>,
          rewritten to use Components by <a href="https://github.com/bbarker">Brandon Elam Barker</a>
          and <a href="https://github.com/olivierBlanvillain">Olivier Blanvillain</a>.
        </p>
        <p>Part of <a href="http://todomvc.com">TodoMVC</a></p>
      </footer>
    </div>
  }

  def main(): Unit = {
    val div = dom.document.getElementById("application-container")
    mount(div, todoapp)
    ()
  }
}
