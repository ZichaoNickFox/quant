module Web.View.NoteView where
import IHP.ViewPrelude
import Web.Types

data NoteView = NoteView
  { noteId :: Id Note
  , noteCells :: [Cell]
  }

instance View NoteView where
  html NoteView { noteId, noteCells } = [hsx|
    <div class="row">
      <div class="col-3 border-end">
        <h5>笔记树</h5>
        <div
          data-tree-root="1"
          data-owner-type="note"
          data-owner-id={tshow noteId}
          style="min-height: 220px;"
        ></div>
      </div>
      <div class="col-9">
        <h3 data-cell-title="1">笔记详情</h3>
        <div data-cell-create="1" data-owner-type="note">
          <input type="hidden" name="ownerType" value="note" />
          <input type="hidden" name="ownerId" value={noteId} data-cell-create-owner-id="1" />
        </div>
        <div data-cell-list="1">
          {forEach noteCells renderCell}
        </div>
      </div>
    </div>
  |]
    where
      renderCell cell = [hsx|
        <div class="card mb-2">
          <div class="card-body">
            <div data-cell-update-card="1" data-owner-type="note">
              <input type="hidden" name="cellId" value={get #id cell} />
              <select name="cellType" class="form-select form-select-sm w-auto mb-2">
                {forEach [Raw, Image, Backtest] (cellTypeOption (get #cellType cell))}
              </select>
              <textarea class="form-control" name="content" rows="4">{fromMaybe "" (get #content cell)}</textarea>
              <div class="d-flex gap-2 flex-wrap mt-2">
                <button class="btn btn-sm btn-outline-secondary" type="button" data-cell-save-button="1">Save</button>
                <button class="btn btn-sm btn-outline-danger" type="button" data-cell-delete-button="1">Delete</button>
                <button class="btn btn-sm btn-outline-secondary" type="button" data-cell-add-above="1">Insert Above</button>
                <button class="btn btn-sm btn-outline-secondary" type="button" data-cell-add-below="1">Insert Below</button>
                <button class="btn btn-sm btn-outline-secondary" type="button" data-cell-move-up="1">Move Up</button>
                <button class="btn btn-sm btn-outline-secondary" type="button" data-cell-move-down="1">Move Down</button>
              </div>
            </div>
          </div>
        </div>
      |]

      cellTypeOption current cellType = [hsx|
        <option value={inputValue cellType} selected={cellType == current}>{show cellType}</option>
      |]
